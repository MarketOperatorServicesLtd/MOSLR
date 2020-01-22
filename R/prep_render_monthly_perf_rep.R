
#' Data Preperation for Monthly Performance Report
#'
#' This function prepares the background data for rendering
#' the Monthly Performance Report.
#'
#' @param dir character
#' @param period.lag numeric
#' @param dir.mps.data character
#' @param dir.mps.sum character
#' @param dir.iprp.comp character
#' @param tracking.mps character
#' @param iprp.plans character
#' @param filter.date date
#' @param csv.outputs character
#' @param dir.output character
#' @param rda.outputs character
#'
#' @return
#' @export
#'
#' @importFrom dplyr rename mutate select filter left_join group_by summarize case_when
#' @importFrom tidyr gather spread
#' @importFrom stats median
#' @importFrom utils read.csv write.csv
#' @importFrom lubridate "%m-%"
#' @importFrom stringr str_sub
#'
#' @examples

prep_render_monthly_perf_rep <- function(
  dir = getwd(),
  period.lag = 1,
  dir.mps.data = paste0(dir, "/data/rdata/mps_data_clean.Rda"),
  dir.mps.sum = paste0(dir, "/data/rdata/mps_data_clean.Rda"),
  dir.iprp.comp = paste0(dir, "/data/rdata/IPRP_plan_comparison_mps.Rda"),
  tracking.mps = paste0(dir, "/data/inputs/tracking_mps.csv"),
  iprp.plans = paste0(dir, "/data/inputs/IPRP_plans_mps.csv"),
  filter.date = "2018-04-01",
  rda.outputs = paste0(dir, "/data/rdata"),
  csv.outputs = paste0(dir, "/data/outputs"),
  dir.output = paste0(dir, "/MonthlyPerfReport")
) {

  period <- Sys.Date() %m-% months(period.lag)
  day(period) <- 1


  # Importing data ----------------------------------------------------------

  mps_data <- readRDS(file = dir.mps.data)
  mps_summary <- readRDS(file = dir.mps.sum)
  iprp_plan_comparison_mps <- readRDS(file = dir.iprp.comp)

  tracking_mps <- read.csv(tracking.mps) %>%
    rename("Trading.Party.ID" = ORG_ID) %>%
    mutate(
      Date = as.Date(Date, "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary),
      key = paste(Trading.Party.ID, MPS)
    )

  IPRP_plans_mps <- read.csv(file = iprp.plans) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
    )


  # Importing monthly tracking sheet ----------------------------------------

  flagged_cols <- c("Trading.Party.ID", "MPS", "Batch", "Action", "Rationale", "Watch_list")

  flagged_mps_table <- tracking_mps %>%
    filter(
      Category == "Performance_Trigger_3m"
      | Category == "Performance_Trigger_6m"
      | (Category == "Watch_list" & !is.numeric(Batch)
      ),
      Date == period
    ) %>%
    mutate(
      Watch_list = (Category == "Watch_list")
    ) %>%
    select(flagged_cols, -Batch)

  saveRDS(flagged_mps_table, file = paste0(rda.outputs, "/flagged_mps_table.Rda"))


  flagged_milestones_mps <- tracking_mps %>%
    filter(
      Category == "Milestone_Trigger"
      | (Category == "Watch_list" & !is.na(Batch)
      ),
      Date == period
    ) %>%
    mutate(
      Watch_list = (Category == "Watch_list")
    ) %>%
    select(flagged_cols)

  saveRDS(flagged_milestones_mps, file = paste0(rda.outputs, "/flagged_milestones_mps.Rda"))


  flagged_iprp_end_mps <- tracking_mps %>%
    filter(
      Category == "IPRP_end",
      Date == period
    ) %>%
    mutate(
      Watch_list = (Category == "Watch_list")
    ) %>%
    select(flagged_cols)

  saveRDS(flagged_iprp_end_mps, file = paste0(rda.outputs, "/flagged_iprp_end_mps.Rda"))

  watch_mps <- tracking_mps %>%
    filter(
      Category == "Watch_list",
      Date == period,
      !is.numeric(Batch)
    ) %>%
    select(Trading.Party.ID, MPS)

  saveRDS(watch_mps, file = paste0(rda.outputs, "/watch_mps.Rda"))


  watch_iprp_mps <- tracking_mps %>%
    filter(
      Category == "Watch_list",
      Date == period,
      !is.na(Batch)
    ) %>%
    select(Trading.Party.ID, MPS, Batch)


  # Combining flagged with mps_data and creating a key ----------------------

  flagged_mps_data <-
    left_join(
      flagged_mps_table,
      mps_data,
      by = c("Trading.Party.ID", "MPS")
    ) %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    filter(
      Watch_list == "No",
      key %in% tracking_mps$key,
      Date >= filter.date
    ) %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      TotalTaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare
    )

  saveRDS(flagged_mps_data, file = paste0(rda.outputs, "/flagged_mps_data"))


  flagged_mps_data_melt <-
    gather(
      flagged_mps_data,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(Trading.Party.ID,-1,-1),
      TaskVolume =
        if_else(
          variable %in% c("MPS_Mean", "MPS_Median", "TaskShare"), 0,
          as.double(TaskVolume)
        ),
      key = factor(key),
      MPS =
        factor(
          MPS, levels = c(
            "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
            "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
            "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19")
        )
    )

  saveRDS(flagged_mps_data_melt, file = paste0(rda.outputs, "/flagged_mps_data_melt.Rda"))


  # IPRP data for tables and graphs -----------------------------------------

  IPRP_tps_mps <- iprp_plan_comparison_mps %>%
    filter(Date == period) %>%
    mutate(key = as.factor(paste(Trading.Party.ID, MPS))) %>%
    select(Trading.Party.ID, MPS, key) %>%
    droplevels()

  IPRP_plans_data_mps <-
    left_join(
      mps_data,
      IPRP_plans_mps,
      by = c("Date", "Trading.Party.ID", "MPS", "key")
    ) %>%
    filter(
      Date >= filter.date,
      key %in% IPRP_tps_mps$key
    ) %>%
    group_by(key) %>%
    mutate(Batch = max(Batch, na.rm = TRUE)) %>%
    ungroup() %>%
    select(
      Date, Trading.Party.ID, MPS, key, Batch,
      TaskCompletion, TaskVolume, TotalTaskVolume,
      MPS_Mean, MPS_Median, TaskShare, Planned_Perf
    )

  cols <- c("Trading.Party.ID", "MPS", "key")

  IPRP_plans_melt_mps <-
    gather(
      IPRP_plans_data_mps,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare, Planned_Perf,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID),-1,-1),
      TaskVolume =
        if_else(
          variable %in% c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"), 0,
          as.double(TaskVolume)
        )
    ) %>%
    mutate_at(cols, factor)

  saveRDS(IPRP_plans_melt_mps, file = paste0(rda.outputs, "/IPRP_plans_melt_mps.Rda"))


  # Preparing data for watch-list graphs using melt--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  watch_mps_data <-
    left_join(
      watch_mps,
      mps_data,
      by = c("Trading.Party.ID", "MPS")
    ) %>%
    filter(Date >="2018-04-01") %>%
    mutate(key = paste(Trading.Party.ID, MPS)) %>%
    select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      TotalTaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare
    )

  watch_mps_melt <-
    gather(
      watch_mps_data,
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare,
      factor_key = TRUE
    ) %>%
    mutate(
      category = str_sub(as.character(Trading.Party.ID),-1,-1),
      TaskVolume =
        if_else(
          variable %in% c("MPS_Mean", "MPS_Median", "TaskShare"),
          0, as.double(TaskVolume)
        ),
      key = as.factor(as.character(key))
    )

  saveRDS(watch_mps_melt, file = paste0(rda.outputs, "/watch_mps_melt.Rda"))


  # Aggregate MPS graphs ----------------------------------------------------

  mps_summary_melt <-
    gather(
      mps_summary,
      key = "variable",
      value = "value",
      MPS_Mean, MPS_Median,
      factor_key = TRUE
    ) %>%
    filter(Date >= filter.date) %>%
    mutate(
      TotalTaskVolume =
        if_else(
          variable == "MPS_Mean", 0,
          as.double(TotalTaskVolume)
        )
    ) %>%
    droplevels()

  saveRDS(mps_summary_melt, file = paste0(rda.outputs, "/mps_summary_melt.Rda"))


  # Preparing IPRP analytics ------------------------------------------------

  iprp_status_mps <- iprp_plan_comparison_mps %>%
    filter(Date == period) %>%
    mutate(
      Status = case_when(
        OnTrack == 1 ~ "On Track",
        Close == 1 ~ "Close",
        OffTrack==1 ~ "Off Track"),
      TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
      Planned_Perf = as.numeric(format(Planned_Perf, digits = 1))
    ) %>%
    select(
      Batch, Trading.Party.ID, MPS, TaskCompletion,
      Planned_Perf, Status
    )

  saveRDS(iprp_status_mps, file = paste0(rda.outputs, "/iprp_status_mps.Rda"))


  # Splitting comparison by category ----------------------------------------

  iprp_plan_comparison_w <- iprp_plan_comparison_mps %>%
    mutate(
      category = str_sub(iprp_plan_comparison_mps$Trading.Party.ID, -1, -1)
    ) %>%
    filter(category == "W", Date == period) %>%
    droplevels()

  w_iprps <- iprp_plan_comparison_w %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      count = n()
    ) %>%
    ungroup() %>%
    spread(
      key = MPS,
      value = count
    ) %>%
    mutate(
      IPRP_count = rowSums(select(., contains("MPS")), na.rm = TRUE)
    )

  write.csv(w_iprps, paste0(csv.outputs, "/w_iprps.csv"))


  iprp_plan_comparison_r <- iprp_plan_comparison_mps %>%
    mutate(
      category = str_sub(iprp_plan_comparison_mps$Trading.Party.ID,-1,-1)
    ) %>%
    filter(category == "R", Date == period) %>%
    droplevels()

  r_iprps <- iprp_plan_comparison_r %>%
    group_by(Trading.Party.ID, MPS) %>%
    summarise(
      count = n()
    ) %>%
    ungroup() %>%
    spread(
      key = MPS,
      value = count
    ) %>%
    mutate(
      Total = rowSums(select(., -(Trading.Party.ID)), na.rm = TRUE),
    )
  r_iprps <- r_iprps %>%
    bind_rows(
      r_iprps %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        mutate(Trading.Party.ID = "Total")
    )

  write.csv(r_iprps, paste0(csv.outputs, "/r_iprps.csv"))

}
