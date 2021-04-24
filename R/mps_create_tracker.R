
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
  period.only = TRUE,
  save.output = TRUE,
  keep.vars = FALSE,
  iprp.list = NULL,
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

  mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))

  Rectification_plans <- utils::read.csv(paste0(my.dir, "/data/inputs/Rectification_plans_mps.csv")) %>%
    dplyr::mutate(Period = as.Date(Period, format = "%d/%m/%Y")) %>%
    dplyr::group_by(Trading.Party.ID, Standard) %>%
    dplyr::mutate(PlanEndDate = max(Period)) %>%
    dplyr::ungroup()

  tracking_sheet <- utils::read.csv(paste0(my.dir, "/data/inputs/tracking_mps.csv")) %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y") %m-% months(-1),
      key = as.factor(paste(Trading.Party.ID, Standard))
      ) %>%
    dplyr::select(Period, Action, key, Template_Sent, Response_Received_Template)


# Creating monthly tracking sheet -------------------------

  monthly_tracking <- mps_data_clean %>%
    dplyr::left_join(Rectification_plans, by = c("Period", "Standard", "Trading.Party.ID", "PerformanceMeasure")) %>%
    dplyr::left_join(tracking_sheet, by = c("Period", "key")) %>%
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
  utils::write.csv(monthly_tracking, paste0(dir.mps.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-mps.csv"), row.names = FALSE)
  saveRDS(monthly_tracking, file = paste0(rda.outputs, "/monthly_tracking_mps_pre.Rda"))
  }

  invisible(monthly_tracking)

}
