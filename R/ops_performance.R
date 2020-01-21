
#' OPS Performance
#'
#' Compares the performance across OPS for a given period
#' for each Trading Party; and compares IPRP performance
#' versus planned milestones.
#'
#' @param period.lag numeric
#' @param ops.list character
#' @param dir character
#' @param dir.ops.data character
#' @param dir.iprp.plans character
#' @param dir.watch.list character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param dir.ops.tracking character
#'
#' @return
#' @export
#'
#' @importFrom dplyr rename mutate select filter left_join group_by summarize
#' @importFrom stats median
#' @importFrom utils read.csv write.csv
#' @importFrom lubridate "%m-%"
#'
#' @examples

ops_performance <- function(
  period.lag = 1,
  ops.list = c("OPS B5a", "OPS C1a"),
  dir = getwd(),
  dir.ops.data = paste0(dir, "/data/rdata/ops_data_clean.Rda"),
  dir.iprp.plans = paste0(dir, "/data/inputs/IPRP_plans_ops.csv"),
  dir.watch.list = paste0(dir, "/data/inputs/tracking_ops.csv"),
  csv.outputs = paste0(dir, "/data/outputs"),
  rda.outputs = paste0(dir, "/data/rdata"),
  dir.ops.tracking = paste0(dir, "/data/tracking/ops")
) {


# Setting parameters ------------------------------------------------------

  period <- Sys.Date() %m-% months(period.lag)
  day(period) <- 1

  period1 <- period
  period2 <- period1 %m-% months(1)
  period3 <- period2 %m-% months(1)
  period6 <- period %m-% months(5)

  dates <- as.Date(c(period1, period2, period3))


# Importing OPS data and OPS IPRP data ------------------------------------

  ops_data_clean <- readRDS(file = dir.ops.data)

  IPRP_plans <- read.csv(dir.iprp.plans) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"))


# IPRP comparison ---------------------------------------------------------

  IPRP_plan_comparison <- left_join(
    IPRP_plans,
    ops_data_clean,
    by = c("Date", "OPS", "Trading.Party.ID")
    ) %>%
    mutate(
      OPS = as.factor(OPS),
      Trading.Party.ID = as.factor(Trading.Party.ID),
      Delta = ifelse(
        indicator == "KPI",
        TaskCompletion - Planned_Perf,
        OutstandingOntime - Planned_Perf
        ),
      DeltaQuant = Delta / Planned_Perf
    ) %>%
    select(
      Trading.Party.ID, OPS, indicator, Date, TaskVolume,
      TaskCompletion, OutstandingOntime, Planned_Perf, Delta, DeltaQuant,
      ops.mean.taskcompletion, ops.mean.outstanding, threshold, Batch, check1,
      check2
    ) %>%
    mutate(
      OnTrack = Delta >= 0,
      Close = (Delta < 0 & DeltaQuant > -0.05),
      OffTrack = (Delta < 0 & DeltaQuant <= -0.05)
    )

  write.csv(
    IPRP_plan_comparison,
    paste0(csv.outputs,"/IPRP_plan_comparison_ops.csv"),
    row.names = FALSE
    )

  saveRDS(
    IPRP_plan_comparison,
    paste0(rda.outputs, "/IPRP_plan_comparison_ops.Rda")
    )


  # Creates IPRP table and IPRP list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  IPRP_table <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(Action = "tbd", Rationale = "tbd") %>%
    select(Trading.Party.ID, OPS, Batch, Action, Rationale, indicator)

  write.csv(
    IPRP_table,
    paste0(csv.outputs, "/flagged_IPRP_table_ops.csv"),
    row.names = FALSE
    )

  IPRP_list <- IPRP_plan_comparison %>%
    mutate(
      key = paste(Trading.Party.ID, OPS)
    ) %>%
    select(key)


# Flagging poor performance -----------------------------------------------

  flagged_ops_table_3m_KPI <- ops_data_clean %>%
    filter(
      Date %in% dates,
      OPS %in% ops.list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      (sum_check1 == 3)
    ) %>%
    mutate(
      indicator = "KPI",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(
      Date, Trading.Party.ID, OPS,
      key, sum_check1, sum_check2, indicator
      )


  flagged_ops_table_3m_API <- ops_data_clean %>%
    filter(
      Date %in% dates,
      OPS %in% ops.list,
      !(key %in% IPRP_list$key)
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      (sum_check2 == 3)
    ) %>%
    mutate(
      indicator = "API",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(
      Date, Trading.Party.ID, OPS,
      key, sum_check1, sum_check2, indicator
      )

  flagged_ops_table_3m <- rbind(flagged_ops_table_3m_API, flagged_ops_table_3m_KPI)

  write.csv(
    flagged_ops_table_3m,
    file = paste0(csv.outputs, "/flagged_ops_table_3m.csv"),
    row.names = FALSE
    )

  saveRDS(
    flagged_ops_table_3m,
    file = paste0(dir, "/data/rdata/flagged_ops_table_3m.Rda")
    )

  flagged_ops_table_6m_KPI <- ops_data_clean %>%
    filter(
      Date >= period6,
      Date <= period,
      OPS %in% ops.list,
      !(key %in% IPRP_list$key),

    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      sum_check1 >= 3
    ) %>%
    mutate(
      indicator = "KPI",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(
      Date, Trading.Party.ID, OPS,
      key, sum_check1, sum_check2, indicator
      )

  flagged_ops_table_6m_API <- ops_data_clean %>%
    filter(
      Date >= period6,
      Date <= period,
      OPS %in% ops.list,
      !(key %in% IPRP_list$key),
    ) %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(
      sum_check1 = sum(check1),
      sum_check2 = sum(check2)
    ) %>%
    ungroup() %>%
    filter(
      sum_check2 >= 3
    ) %>%
    mutate(
      indicator = "API",
      Batch = NA, Action = "tbd", Rationale = "tbd",
      key = as.factor(paste(Trading.Party.ID, OPS, indicator)),
      Date = period
    ) %>%
    droplevels() %>%
    select(
      Date, Trading.Party.ID, OPS,
      key, sum_check1, sum_check2, indicator
      )

  flagged_ops_table_6m <- rbind(flagged_ops_table_6m_API, flagged_ops_table_6m_KPI)

  flagged_ops_table_6m <- flagged_ops_table_6m %>%
    filter(!(key %in% flagged_ops_table_3m$key))

  write.csv(
    flagged_ops_table_6m,
    file = paste0(csv.outputs, "/flagged_ops_table_6m.csv"),
    row.names = FALSE
    )

  saveRDS(
    flagged_ops_table_6m,
    file = paste0(rda.outputs, "/flagged_ops_table_6m.Rda")
    )

  # Creating Watch-list --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  watch_list <- read.csv(paste0(dir, "/data/inputs/tracking_ops.csv")) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, OPS))
    ) %>%
    filter(Date == period2, Action == "Watch") %>%
    droplevels() %>%
    select(Date, Trading.Party.ID, OPS, Batch, Action, Rationale, key, indicator)

  write.csv(watch_list, paste0(dir, "/data/outputs/watch_list_ops.csv"), row.names = FALSE)
  saveRDS(watch_list, file = paste0(dir, "/data/rdata/watch_list_ops.Rda"))


  # Combining flagged OPS, IPRP milestone flags and Watch-list into one tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

  flagged_ops_table_3m <- flagged_ops_table_3m %>%
    mutate(
      Category =
        case_when(
          indicator == "KPI" ~ "Performance_Trigger_3m_KPI",
          indicator == "API" ~ "Performance_Trigger_3m_API",
          TRUE ~ "Other"
        ),
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS),
      key = paste(Trading.Party.ID, OPS, indicator)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch, indicator, key)

  flagged_ops_table_6m <- flagged_ops_table_6m %>%
    mutate(
      Category =
        case_when(
          indicator == "KPI" ~ "Performance_Trigger_6m_KPI",
          indicator == "API" ~ "Performance_Trigger_6m_API",
          TRUE ~ "Other"
        ),
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS),
      key = paste(Trading.Party.ID, OPS, indicator)
    ) %>%
    droplevels() %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch, indicator, key)

  watch_list <- watch_list %>%
    mutate(
      Date = period,
      Category = "Watch_list",
      Batch = NA,
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS),
      key = paste(Trading.Party.ID, OPS, indicator)
    ) %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch, indicator, key)

  IPRP_plan_comparison <- IPRP_plan_comparison %>%
    filter(Date == period, OffTrack == 1) %>%
    mutate(
      Category = "Milestone_Trigger",
      Trading.Party.ID = as.character(Trading.Party.ID),
      OPS = as.character(OPS),
      key = paste(Trading.Party.ID, OPS, indicator)
    ) %>%
    select(Category, Date, Trading.Party.ID, OPS, Batch, indicator, key)

  iprp_end <- IPRP_plans %>%
    group_by(Trading.Party.ID, OPS) %>%
    summarise(end.date = max(Date)) %>%
    ungroup() %>%
    filter(end.date == period) %>%
    mutate(Category = "IPRP_end")

  monthly_tracking <-
    rbind(
      watch_list,
      flagged_ops_table_3m,
      flagged_ops_table_6m,
      IPRP_plan_comparison,
      iprp_end
    ) %>%
    mutate_at(
      c("Category", "Trading.Party.ID", "OPS", "Batch"),
      factor
    )%>%
    select(Category, Date, Trading.Party.ID, OPS, Batch, indicator)

  write.csv(
    monthly_tracking,
    file = paste0(dir.ops.tracking, "/", format(period, "%Y-%m"), "_monthly-tracking-ops.csv"),
    row.names = FALSE
    )

  saveRDS(
    monthly_tracking,
    file = paste0(rda.outputs, "/monthly-tracking-ops.Rda")
    )

}
