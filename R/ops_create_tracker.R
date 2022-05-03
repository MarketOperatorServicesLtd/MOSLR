
#' Create Monthly OPS Tracker (Pre Analysis and PfM Commentary)
#'
#' Compares the performance across OPS for each Trading Party
#' by OPS and compares IPRP performance versus planned milestones.
#' This is done prior to analysis and Pfm commentary being added.
#'
#' @param period date
#' @param my.dir character
#' @param rda.outputs character
#' @param period.only logical
#' @param save.output logical
#' @param keep.vars logical
#' @param filter.category character
#' @param dir.ops.tracking character
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

ops_create_tracker <- function(
  period = Sys.Date() %m-% months(1),
  my.dir = getwd(),
  conf.loc = NULL,
  rda.outputs = paste0(my.dir, "/data/rdata"),
  dir.ops.tracking = paste0(my.dir, "/data/tracking/ops"),
  period.only = TRUE,
  save.output = TRUE,
  filter.category = c("IPRP: On-track", "PRP: On-track", "IPRP: Above plan", "PRP: Above plan", "Normal monitoring"),
  keep.vars = FALSE,
  DataBase = TRUE
) {

  # Setting parameters ------------------------------------------------------

  period <- as.Date(period)

  lubridate::day(period) <- 1

  if (keep.vars) {
    var_list <- NULL
  } else {
    var_list <-
      c(
        "Period", "SecondaryCategory", "Trading.Party.ID", "Standard",
        "PerformanceMeasure",
        "Action","Rationale", "PFM_Commentary", "ActiveIPRP", "ActivePRP", "IPRPend", "PRPend",
        "MilestoneFlag", "PerfFlag3m", "PerfFlag6m", "OnWatch",
        "OnWatchRectificationEnd",  "Consistency", "PerfRating"
      )
  }


  # Importing data ----------------------------------------------------------



  Sys.setenv(R_CONFIG_ACTIVE = "digitaldata")

  if(is.null(conf.loc)){
    err <-  try(conf <- config::get(), TRUE)
    if("try-error" %in% class(err)) conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else if( conf.loc == "select"){
    conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else{
    conf <- config::get(file = conf.loc)
  }

  bl_endp_key <- AzureStor::storage_endpoint(endpoint = conf$endpoint, sas = conf$sas)
  cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")

  if(DataBase){

    ops_data_clean <- AzureStor::storage_read_csv(cont, "PerfReports/data/inputs/OPS_data_clean.csv") %>%
      dplyr::mutate(
        Period = as.Date(Period),
        Threshold = as.numeric(Threshold),
        #Details = iconv(Details),
        #Context = iconv(Context)
      )


    # ops_data_clean <- dplyr::tbl(con, "PERF_OPSDataClean") %>%
    #   dplyr::as_tibble(
    #   ) %>%
    #   dplyr::mutate(
    #     Period = as.Date(Period),
    #     Threshold = as.numeric(Threshold),
    #     Details = iconv(Details),
    #     Context = iconv(Context)
    #     )
  } else{
    ops_data_clean <- readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda"))

  }



  IPRP_plans <-
    AzureStor::storage_read_csv(cont,
      "PerfReports/data/inputs/IPRP_plans_ops.csv"
      ) %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y")
      ) %>%
    dplyr::group_by(
      Trading.Party.ID, Standard, PerformanceMeasure
      ) %>%
    dplyr::mutate(
      PlanEndDate = max(Period)
      ) %>%
    dplyr::ungroup()



  tracking_sheet <- AzureStor::storage_read_csv(cont, paste0("/PerfReports/data/inputs/tracking_ops.csv"))%>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y") %m-% months(-1)
    ) %>%
    dplyr::select(
      Period, Action, Trading.Party.ID, Standard, PerformanceMeasure, Template_Sent, Response_Received_Template
    )


  # Creating monthly tracking sheet -------------------------

  monthly_tracking <- ops_data_clean %>%
    dplyr::left_join(
      IPRP_plans,
      by = c("Period", "Standard", "Trading.Party.ID", "PerformanceMeasure")
      ) %>%
    dplyr::left_join(
      tracking_sheet,
      by = c("Period", "Trading.Party.ID", "Standard", "PerformanceMeasure")
      ) %>%
    dplyr::mutate(
      Action = tolower(Action),
      Delta = Performance - Planned_Perf,
      DeltaQuant = Delta / Planned_Perf,
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
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
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
    )%>%
    {if (!keep.vars) {
      dplyr::select(., var_list)
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

    if(!DataBase){
    utils::write.csv(monthly_tracking,
                     paste0(dir.ops.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-ops.csv"),
                     row.names = FALSE)
    saveRDS(monthly_tracking,
            file = paste0(rda.outputs, "/monthly_tracking_ops_pre.Rda")
            )

    }

    AzureStor::storage_write_csv(monthly_tracking,
                                 cont,
                                 paste0("PerfReports/tracking/",  format(period, "%Y-%m"), "_monthly-tracking-ops.csv")
                                 )
    }


  invisible(monthly_tracking)

}
