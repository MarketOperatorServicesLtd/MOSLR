
#' Create Monthly MPS Tracker (Pre Analysis and PfM Commentary)
#'
#' Compares the performance across MPS for each Trading Party
#' by MPS and compares IPRP performance versus planned milestones.
#' This is done prior to analysis and Pfm commentary being added.
#'
#' @param mps_list character
#' @param rda.outputs character
#' @param dir.mps.tracking character
#' @param my.dir character
#' @param period date
#' @param period.only logical
#' @param save.output  logical
#' @param keep.vars logical
#'
#' @return
#' @export
#'
#' @importFrom lubridate "%m-%"
#'
#' @examples

create_monthly_tracker <- function(
  period = Sys.Date() %m-% months(1),
  mps_list =
    c("MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 7",
      "MPS 12", "MPS 16", "MPS 17", "MPS 18"
      ),
  my.dir = getwd(),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  dir.mps.tracking = paste0(my.dir, "/data/tracking/mps"),
  period.only = TRUE,
  save.output = TRUE,
  keep.vars = FALSE
  ) {

# Setting parameters ------------------------------------------------------

  lubridate::day(period) <- 1

  if (keep.vars) {
    var_list <- NULL
    } else {
      var_list <-
        c(
        "Date", "Category", "Trading.Party.ID", "MPS", "Action",
        "Rationale", "PFM_Commentary", "ActiveIPRP", "IPRPend",
        "MilestoneFlag", "PerfFlag3m", "PerfFlag6m", "OnWatch",
        "OnWatchIPRPend",  "Consistency", "PerfRating"
        )
      }


# Importing data ----------------------------------------------------------

  mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))

  IPRP_plans <- utils::read.csv(paste0(my.dir, "/data/inputs/IPRP_plans_mps.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y")
      ) %>%
    dplyr::group_by(
      Trading.Party.ID, MPS
      ) %>%
    dplyr::mutate(
      PlanEndDate = max(Date),
      IPRPend = PlanEndDate == Date
      ) %>%
    ungroup()

  tracking_sheet <- utils::read.csv(paste0(my.dir, "/data/inputs/tracking_mps.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
      ) %>%
    dplyr::select(
      Date, Action, key
    )


# Creating monthly tracking sheet -------------------------

  monthly_tracking <- mps_data_clean %>%
    dplyr::left_join(
      IPRP_plans,
      by = c("Date", "MPS", "Trading.Party.ID")
      ) %>%
    dplyr::left_join(
      tracking_sheet,
      by = c("Date", "key")
      ) %>%
    dplyr::mutate(
      Delta = TaskCompletion - Planned_Perf,
      DeltaQuant = Delta / Planned_Perf,
      MilestoneFlag = TaskCompletion < Planned_Perf,
      TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
      Planned_Perf = as.numeric(format(Planned_Perf, digits = 1)),
      Status = case_when(
        (DeltaQuant > 0.05) ~ "Outperform",
        (DeltaQuant <= 0.05 & DeltaQuant >= -0.05) ~ "OnTrack",
        (DeltaQuant < -0.05) ~ "OffTrack"
        ),
      ActiveIPRP = !is.na(Status),
      MilestoneFlag = Status == "OffTrack",
      OnWatch = Action == "Watch",
      OnWatchIPRPend = (Action == "De-escalate" | Action == "IPRP_end")
      ) %>%
    droplevels() %>%
    dplyr::arrange(Trading.Party.ID, MPS, Date) %>%
    dplyr::group_by(Trading.Party.ID, MPS) %>%
    dplyr::mutate(
      PerfFlag3m = zoo::rollapply(BelowPeer, 3, mean, align = "right", fill = NA) == 1,
      PerfFlag6m = zoo::rollapply(BelowPeer, 6, mean, align = "right", fill = NA) >= 0.5,
      rolling.sd = zoo::rollapply(TaskCompletion, 6, sd, align = "right", fill = NA),
      rolling.mean = zoo::rollapply(TaskCompletion, 6, mean, align = "right", fill = NA),
      Consistency =
        dplyr::case_when(
          rolling.sd < 0.01 ~ "Very Consistent",
          rolling.sd >= 0.01 & rolling.sd < 0.02 ~ "Consistent",
          rolling.sd >= 0.02 & rolling.sd < 0.05 ~ "Inconsistent",
          rolling.sd > 0.05 ~ "Very Inconsistent",
          TRUE ~ "Insufficient data"
          ),
      PerfRating =
        dplyr::case_when(
          rolling.mean >= 0.9 ~ "Very Good",
          rolling.mean < 0.9 & rolling.mean >= 0.8 ~ "Good",
          rolling.mean < 0.8 & rolling.mean >= 0.7 ~ "Poor",
          rolling.mean < 0.7 ~ "Very Poor",
          TRUE ~ "Insufficient data"
        )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Category =
      case_when(
        OnWatch ~ "Watch_list",
        #PerfFlag6m ~ "Performance_Trigger_6m",
        PerfFlag3m ~ "Performance_Trigger_3m",
        MilestoneFlag ~ "Milestone_Trigger",
        IPRPend ~ "IPRP_end",
        OnWatchIPRPend ~ "IPRP_end_watch",
        TRUE ~ "None"
        ),
      Action = "tbd",
      Rationale = "tbd",
      PFM_Commentary = "tbd"
      ) %>%
    {if (!keep.vars) {
      dplyr::select(., var_list)
      } else {
        dplyr::select(., dplyr::everything())
        }
      } %>%
    {if (period.only) {
      dplyr::filter(., Date == period)
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
