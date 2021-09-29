
#' Create Monthly MPS Tracker (Pre Analysis and PfM Commentary)
#'
#' Compares the performance across MPS for each Trading Party
#' by MPS and compares IPRP performance versus planned milestones.
#' This is done prior to analysis and Pfm commentary being added.
#'
#' @param rda.outputs character
#' @param dir.mps.tracking character
#' @param my.dir character
#' @param period date
#' @param period.only logical
#' @param save.output logical
#' @param keep.vars logical
#' @param iprp.list character
#' @param filter.category character
#'
#' @return
#' @export
#'
#' @importFrom lubridate "%m-%"
#' @importFrom magrittr %>%
#'
#' @examples

mps_create_tracker <- function(
  period = Sys.Date() %m-% months(1),
  my.dir = getwd(),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  dir.mps.tracking = paste0(my.dir, "/data/tracking/mps"),
  StandardKey = "MPS",
  period.only = TRUE,
  save.output = TRUE,
  keep.vars = FALSE,
  iprp.list = NULL,
  DataBase = TRUE,
  filter.category = c("IPRP: On-track", "PRP: On-track", "IPRP: Above plan", "PRP: Above plan", "Normal monitoring", "Performance flag: 6 month")
  ) {

# Setting parameters ------------------------------------------------------

  period <- as.Date(period)

  lubridate::day(period) <- 1

  if (keep.vars) {
    var_list <- NULL
    } else {
      var_list <-
        c(
        "Period", "SecondaryCategory", "Trading.Party.ID", "PerformanceMeasure", "Standard", "Action",
        "Rationale", "PFM_Commentary", "ActiveIPRP", "ActivePRP", "IPRPend", "PRPend",
        "MilestoneFlag", "PerfFlag3m", "PerfFlag6m", "OnWatch",
        "OnWatchRectificationEnd",  "Consistency", "PerfRating"
        )
      }


# Importing data ----------------------------------------------------------

  if(DataBase){
    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "SQL Server",
                           Server = "data-mgmt",
                           Database = "MOSL_Sandpit",
                           Port = 1433,
                           trusted_connection = "True")
    if(StandardKey == 'MPS'){
      data_clean <- dplyr::tbl(con, "PERF_MPSDataClean") %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Period = as.Date(Period),
                      Threshold = as.numeric(Threshold))

    } else if (StandardKey == 'API'){
      data_clean <- dplyr::tbl(con, "PERF_APIDataClean") %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Period = as.Date(Period),
                      Threshold = as.numeric(Threshold))
    } else {
      stop("Variable StandardKey has to be either 'MPS' or 'API'")}
  } else{

    if(StandardKey == 'MPS'){
    data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
  } else if (StandardKey == 'API'){
    data_clean <- read.csv(paste0(my.dir, "/data/outputs/API_data_clean.csv"), fileEncoding="UTF-8-BOM") %>%
    dplyr::mutate(Period = as.Date(Period),
                  Threshold = as.numeric(Threshold))
  } else {
    stop("Variable StandardKey has to be either 'MPS' or 'API'")}
  }




  endpoint_url <- "https://stmosldataanalyticswe.blob.core.windows.net/"
  sas <- readr::read_file(ifelse(file.exists(paste0(my.dir, "/data/inputs/digitaldata_sas.txt")), paste0(my.dir, "/data/inputs/digitaldata_sas.txt"), choose.files()))
  bl_endp_key <- AzureStor::storage_endpoint(endpoint = endpoint_url, sas = sas)
  cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")

  Rectification_plans <- AzureStor::storage_read_csv(cont, paste0("PerfReports/Rectification_plans_", tolower(StandardKey), ".csv")) %>%
    dplyr::mutate(Period = as.Date(Period, format = "%d/%m/%Y")) %>%
    dplyr::group_by(Trading.Party.ID, Standard) %>%
    dplyr::mutate(PlanEndDate = max(Period)) %>%
    dplyr::ungroup()

  tracking_sheet <- AzureStor::storage_read_csv(cont, paste0("/PerfReports/tracking_", tolower(StandardKey), ".csv")) %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y") %m-% months(-1),
      key = as.factor(paste(Trading.Party.ID, Standard))
    ) %>%
    dplyr::select(Period, Action, key, Template_Sent, Response_Received_Template)





# Creating monthly tracking sheet -------------------------

  monthly_tracking <- data_clean %>%
    dplyr::left_join(Rectification_plans, by = c("Period", "Standard", "Trading.Party.ID", "PerformanceMeasure")) %>%
    dplyr::left_join(tracking_sheet, by = c("Period", "key")) %>%
    dplyr::mutate(
      Action = tolower(Action),
      Delta = Performance - Planned_Perf,
      DeltaQuant = Delta / (Planned_Perf+0.01),
      Status = dplyr::case_when(
        (DeltaQuant > 0.05) ~ paste0(RectificationType, ": Above plan"),
        (DeltaQuant <= 0.05 & DeltaQuant >= -0.05) ~ paste0(RectificationType, ": On-track"),
        (DeltaQuant < -0.05) ~ paste0(RectificationType, ": Below plan")
        ),
      OnWatch = Action == "watch: performance",
      OnWatchRectificationEnd = Action == "de-escalate" | Action == "watch: iprp end",
      MilestoneFlag = Performance < Planned_Perf,
      IPRPend = PlanEndDate == Period & RectificationType == "IPRP",
      PRPend = PlanEndDate == Period & RectificationType == "PRP",
      Pending = Template_Sent != "" & Response_Received_Template == "",
      UnderReview =
        Action == "review" | Action == "re-submit" | Action == "extend" | Action == "escalate",
      ActiveIPRP = RectificationType == "IPRP",
      ActivePRP = RectificationType == "PRP",
      InactiveRectification = IPRPend | PRPend | Pending | (UnderReview & !ActiveIPRP & !ActivePRP),
      Rectification = ActiveIPRP | ActivePRP | InactiveRectification
      ) %>%
    droplevels() %>%
    dplyr::arrange(Trading.Party.ID, Standard, Period) %>%
    dplyr::group_by(Trading.Party.ID, Standard) %>%
    dplyr::mutate(
      PerfFlag3m = zoo::rollapply(BelowPeer, 3, mean, align = "right", fill = NA) == 1,
      PerfFlag6m = zoo::rollapply(BelowPeer, 6, mean, align = "right", fill = NA) >= 0.5,
      rolling.sd = zoo::rollapply(Performance, 6, stats::sd, align = "right", fill = NA),
      rolling.mean = zoo::rollapply(Performance, 6, mean, align = "right", fill = NA),
      Consistency =
        dplyr::case_when(
          TaskVolume <= 20 ~ "Low or No Tasks",
          rolling.sd < 0.01 ~ "Very Consistent",
          rolling.sd >= 0.01 & rolling.sd < 0.02 ~ "Consistent",
          rolling.sd >= 0.02 & rolling.sd < 0.05 ~ "Variable",
          rolling.sd > 0.05 ~ "Highly Variable",
          TRUE ~ "Insufficient data"
          ),
      PerfRating =
        dplyr::case_when(
          rolling.mean >= 0.9 ~ "Very Good",
          rolling.mean < 0.9 & rolling.mean >= 0.8 ~ "Good",
          rolling.mean < 0.8 & rolling.mean >= 0.7 ~ "Poor",
          rolling.mean < 0.7 ~ "Very Poor",
          TRUE ~ "Insufficient data"
          ),
      SumPerf3m = tidyr::replace_na(zoo::rollapply(PerfFlag3m, 12, sum, align = "right", fill = NA), 0),
      SumPerf6m = tidyr::replace_na(zoo::rollapply(PerfFlag6m, 12, sum, align = "right", fill = NA), 0)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.logical, ~tidyr::replace_na(., FALSE)) %>%
    dplyr::mutate(
      PrimaryCategory =
        dplyr::case_when(
          !Rectification ~ "Monitoring",
          Rectification ~ "Resolution"
        ),
      SecondaryCategory =
        dplyr::case_when(
          OnWatch ~ "Watch: performance",
          OnWatchRectificationEnd ~ "Watch: Rectification end",
          !Rectification & !OnWatch & PerfFlag6m & !PerfFlag3m  & IPRPeligible ~ "Performance flag: 6 month",
          !Rectification & !OnWatch & PerfFlag3m  & IPRPeligible ~ "Performance flag: 3 month",
          ActiveIPRP & !IPRPend ~ "Rectification: IPRP",
          ActivePRP & !PRPend ~ "Rectification: PRP",
          PRPend ~ "PRP: end",
          IPRPend ~ "IPRP: end",
          Rectification & UnderReview ~ "Rectification: under review",
          TRUE ~ "Normal monitoring"
          ),
      Action = "tbd",
      Rationale = "tbd",
      PFM_Commentary = "tbd"
      ) %>%
    dplyr::filter(
      !(SecondaryCategory %in% filter.category),
      !Status %in% filter.category
      ) %>%
    {if (!keep.vars) {
      dplyr::select(., all_of(var_list))
      } else {
        dplyr::select(., dplyr::everything())
        }
      } %>%
    {if (period.only) {
      dplyr::filter(., Period == period)
      } else {
        dplyr::mutate(., date.stamp = Sys.Date())
      }
      }

  if (save.output) {
  utils::write.csv(monthly_tracking, paste0(dir.mps.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-", tolower(StandardKey), ".csv"), row.names = FALSE)
  saveRDS(monthly_tracking, file = paste0(rda.outputs, "/monthly_tracking_", tolower(StandardKey), "_pre.Rda"))
  AzureStor::storage_write_csv(monthly_tracking, cont, paste0("PerfReports/tracking/",  format(period, "%Y-%m"), "_monthly-tracking-", tolower(StandardKey), ".csv"))
}
  if(DataBase) odbc::dbDisconnect(con)
  invisible(monthly_tracking)

}
