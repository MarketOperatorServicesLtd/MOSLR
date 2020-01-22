
#' MPS Performance
#'
#' Compares the performance across OPS for a given period
#' for each Trading Party; and compares IPRP performance
#' versus planned milestones.
#'
#' @param period.lag numeric
#' @param mps_list character
#' @param rda.outputs character
#' @param dir.mps.tracking character
#' @param current.month.only logical
#' @param my.dir character
#'
#' @return
#' @export
#'
#' @importFrom lubridate "%m-%"
#'
#' @examples

mps_performance <- function(
  period.lag = 1,
  mps_list = c(
    "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5",
    "MPS 7", "MPS 12", "MPS 16", "MPS 17", "MPS 18"
    ),
  my.dir = getwd(),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  dir.mps.tracking = paste0(my.dir, "/data/tracking/mps"),
  current.month.only = TRUE
  ) {

# Setting parameters ------------------------------------------------------

  period <- Sys.Date() %m-% months(period.lag)
  lubridate::day(period) <- 1

  period1 <- period
  period2 <- period1 %m-% months(1)
  period3 <- period2 %m-% months(1)
  period6 <- period %m-% months(5)

  ds <- as.Date(c(period1, period2, period3))


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

  tracking_sheet <- utils::read.csv(paste0(my.dir, "/data/inputs/tracking_mps.csv"))


# Creating watch-list or IPRP end review from tracker at beginning of period -----------------

  watch_list <- tracking_sheet %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
      ) %>%
    dplyr::filter(
      Date == period2,
      Action == "Watch"
    )

  watch_list_iprp_end <- tracking_sheet %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
      ) %>%
    dplyr::filter(
      Date == period2,
      (Action == "IPRP_end" | Action == "De-escalate")
    )


# IPRP Plan Comparison: Milestones versus actuals -------------------------

  monthly_tracking <- mps_data_clean %>%
    dplyr::left_join(
      IPRP_plans,
      by = c("Date", "MPS", "Trading.Party.ID")
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
      OnWatch = key %in% watch_list$key,
      OnWatchIPRPend = key %in% watch_list_iprp_end$key
      ) %>%
    droplevels() %>%
    dplyr::arrange(Trading.Party.ID, MPS, Date) %>%
    dplyr::group_by(Trading.Party.ID, MPS) %>%
    dplyr::mutate(
      PerfFlag3m = zoo::rollapply(BelowPeer, 3, mean, align = "right", fill = NA) == 1,
      PerfFlag6m = zoo::rollapply(BelowPeer, 6, mean, align = "right", fill = NA) >= 0.5,
      rolling.sd = zoo::rollapply(TaskCompletion, 6, sd, align = "right", fill = NA),
      Consistency = dplyr::case_when(
        rolling.sd < 0.01 ~ "Very Consistent",
        rolling.sd >= 0.01 & rolling.sd < 0.02 ~ "Consistent",
        rolling.sd >= 0.02 & rolling.sd < 0.05 ~ "Inconsistent",
        rolling.sd > 0.05 ~ "Very Inconsistent"
      )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Category =
      case_when(
        OnWatch ~ "Watch_list",
        PerfFlag6m ~ "Performance_Trigger_6m",
        PerfFlag3m ~ "Performance_Trigger_3m",
        MilestoneFlag ~ "Milestone_Trigger",
        IPRPend ~ "IPRP_end",
        OnWatchIPRPend ~ "IPRP_end_watch",
        TRUE ~ "None"
      ),
      Action = "tbd",
      Rationale = "tbd"
      ) %>%
    dplyr::select(
      Category, Date, Trading.Party.ID, MPS, Action,
      Rationale, ActiveIPRP, IPRPend, MilestoneFlag,
      OnWatch, OnWatchIPRPend, PerfFlag3m, PerfFlag6m,
      Consistency
      ) %>%
    {if (current.month.only) {
    dplyr::filter(., Date == period)
    } else {
      dplyr::mutate(., date.stamp = period)
    }
    }

  utils::write.csv(monthly_tracking, paste0(dir.mps.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-mps.csv"), row.names = FALSE)
  saveRDS(monthly_tracking, file = paste0(rda.outputs, "/monthly-tracking-mps.Rda"))

  invisible(monthly_tracking)

}
