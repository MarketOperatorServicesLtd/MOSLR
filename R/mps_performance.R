#' MPS Performance
#'
#' @param period.lag numeric
#' @param mps_list character
#' @param dir character
#' @param dir.mps.data character
#' @param dir.iprp.plans character
#' @param dir.watch.list character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param dir.mps.tracking character
#'
#' @return
#' @export
#'
#' @examples

mps_performance <- function(
  period.lag = 1,
  mps_list = c(
    "MPS 1", "MPS 2", "MPS 3", "MPS 4",
    "MPS 7", "MPS 12", "MPS 16", "MPS 17",
    "MPS 18"
    ),
  dir = getwd(),
  dir.mps.data = paste0(dir, "/data/rdata/mps_data_clean.Rda"),
  dir.iprp.plans = paste0(dir, "/data/inputs/IPRP_plans_mps.csv"),
  dir.watch.list = paste0(dir, "/data/inputs/tracking_mps.csv"),
  csv.outputs = paste0(dir, "/data/outputs"),
  rda.outputs = paste0(dir, "/data/rdata"),
  dir.mps.tracking = paste0(dir, "/data/tracking/mps")
  ) {

# Setting parameters ------------------------------------------------------

  period <- Sys.Date() %m-% months(period.lag)
  day(period) <- 1

  period1 <- period
  period2 <- period1 %m-% months(1)
  period3 <- period2 %m-% months(1)
  period6 <- period %m-% months(5)

  ds <- as.Date(c(period1, period2, period3))


# Importing data ----------------------------------------------------------

  mps_data_clean <- readRDS(dir.mps.data)

  IPRP_plans <- read.csv(dir.iprp.plans) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"))


# IPRP Plan Comparison: Milestones versus actuals -------------------------

  IPRP_plan_comparison <-
    left_join(
      IPRP_plans,
      mps_data_clean,
      by = c("Date", "MPS", "Trading.Party.ID")
    ) %>%
    mutate(
      MPS = as.factor(MPS),
      Trading.Party.ID = as.factor(Trading.Party.ID),
      Delta = TaskCompletion - Planned_Perf,
      DeltaQuant = Delta / Planned_Perf,
      check2 = if_else ((TaskCompletion < Planned_Perf), 1, 0)
    ) %>%
    select(
      Trading.Party.ID, MPS, Date, TaskVolume,
      TaskCompletion, Planned_Perf, Delta, DeltaQuant,
      MPS_Mean, MPS_Median, mps_threshold, Batch, check1,
      check2
    ) %>%
    mutate(
      OnTrack = Delta >= 0,
      Close = Delta < 0 & DeltaQuant > -0.05,
      OffTrack = Delta < 0 & DeltaQuant <= -0.05
    )

  write.csv(IPRP_plan_comparison, paste0(csv.outputs, "/IPRP_plan_comparison.csv"))
  saveRDS(IPRP_plan_comparison, paste0(rda.outputs, "/IPRP_plan_comparison_mps.Rda"))


# IPRP Tables -------------------------------------------------------------

  IPRP_table <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(Action = "tbd", Rationale = "tbd") %>%
    select(Trading.Party.ID, MPS, Batch, Action, Rationale)

  write.csv(IPRP_table, paste0(csv.outputs, "/flagged_IPRP_table_mps.csv"))

  IPRP_list <- IPRP_plan_comparison %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    select(key)


# Flagging poor performance -----------------------------------------------

  flagged_mps_table_3m <- mps_data_clean %>%
    filter(
      Date %in% ds,
      MPS %in% mps_list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      sum_check1 = sum(check1)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 == 3
    ) %>%
    mutate(
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, MPS)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, MPS, key)

  write.csv(flagged_mps_table_3m, paste0(csv.outputs, "/flagged_mps_table_3m.csv"))
  saveRDS(flagged_mps_table_3m, file = paste0(rda.outputs, "/flagged_mps_table_3m.Rda"))


  flagged_mps_table_6m <- mps_data_clean %>%
    filter(
      Date >= period6,
      MPS %in% mps_list,
      !(key %in% IPRP_list$key),
      !(key %in% flagged_mps_table_3m$key)
    ) %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      sum_check1 = sum(check1)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 >= 3
    ) %>%
    mutate(
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, MPS)),
      Date = period
    ) %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, MPS, key)

  write.csv(flagged_mps_table_6m, paste0(csv.outputs, "/flagged_mps_table_6m.csv"))
  saveRDS(flagged_mps_table_6m, file = paste0(rda.outputs, "/flagged_mps_table_6m.Rda"))


# Creating watch-list -----------------------------------------------------

  watch_list <- read.csv(dir.watch.list) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
    ) %>%
    filter(
      Date == period2,
      (Action == "Watch" | Action == "De-escalate")
      ) %>%
    droplevels() %>%
    select(
      Date, Trading.Party.ID, MPS,
      Batch, Action, Rationale, key
      )

  write.csv(watch_list, paste0(csv.outputs, "/watch_list_mps.csv"))
  saveRDS(watch_list, file = paste0(rda.outputs, "/watch_list_mps.Rda"))


# Combining flagged MPS, IPRP milestone flags and Watch-list --------------

  flagged_mps_table_3m <- flagged_mps_table_3m %>%
    mutate(
      Category = "Performance_Trigger_3m",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    droplevels() %>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch, key
      )

  flagged_mps_table_6m <- flagged_mps_table_6m %>%
    mutate(
      Category = "Performance_Trigger_6m",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    droplevels() %>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch, key
      )

  watch_list <- watch_list %>%
    mutate(
      Date = period,
      Category = "Watch_list",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch, key
      )

  IPRP_plan_comparison_period <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(
      Category = "Milestone_Trigger",
      Trading.Party.ID = as.character(Trading.Party.ID),
      MPS = as.character(MPS),
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch, key
      )

  iprp_end <- IPRP_plans %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(end.date = max(Date)) %>%
    ungroup() %>%
    filter(end.date == period) %>%
    mutate(
      Category = "IPRP_end",
      Batch = NA,
      Date = period,
      key = paste(Trading.Party.ID, MPS)
    ) %>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch, key
      )


  watch_list_bind <- watch_list %>%
    filter(!key %in% c(
      flagged_mps_table_3m$key,
      flagged_mps_table_6m$key,
      IPRP_plan_comparison_period$key,
      iprp_end$key)
    )

  monthly_tracking <-
    rbind(
      watch_list_bind,
      flagged_mps_table_3m,
      flagged_mps_table_6m,
      IPRP_plan_comparison_period,
      iprp_end
    ) %>%
    mutate_at(
      c("Category", "Trading.Party.ID", "MPS", "Batch"),
      factor
    )%>%
    select(
      Category, Date, Trading.Party.ID,
      MPS, Batch
      )

  write.csv(monthly_tracking, paste0(dir.mps.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-mps.csv"))
  saveRDS(monthly_tracking, file = paste0(rda.outputs, "/monthly-tracking-mps.Rda"))

}
