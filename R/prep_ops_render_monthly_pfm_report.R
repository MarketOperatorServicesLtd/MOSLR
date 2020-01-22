
prep_ops_render_monthly_pfm_report <- function(
  dir = getwd(),
  dir2 = choose.dir(),
  start.date = 12,
  dir.ops.data = paste0(dir, "/data/rdata/ops_data_clean.Rda"),
  ops.filter.date = "2019-04-01"
  ) {


# Importing OPS data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

start <- Sys.Date() %m-% months(start.date)
lubridate::day(start) <- 1

ops_data <- readRDS(file = dir.ops.data) %>%
  dplyr::filter(
    Date >= ops.filter.date,
    TaskVolume > 0
  ) %>%
  dplyr::mutate(
    threshold.taskcompletion = ifelse (OPS %in% c("OPS B5a", "OPS C1a"),0.85, NA),
    threshold.outstanding = ifelse (OPS %in% c("OPS B5a", "OPS C1a"),0.75, NA))


ops_data_KPI <- ops_data%>%
  mutate(key=paste(key,"KPI"))%>%
  filter(TaskVolume > 0)

ops_data_API  <- ops_data%>%
  mutate(key=paste(key,"API"))%>%
  filter(TotalOutstanding > 0)

ops_summary <- readRDS(file = paste(dir, "/data/rdata/ops_summary.Rda", sep = "")) %>%
  filter(Date >= start) %>%
  mutate(
    Date = format(Date, "%Y-%m"),
    ops.mean.taskcompletion = ops.mean.taskcompletion * 100,
    OPS_Median = OPS_Median * 100)


# Melting OPS data for graphs --------------------------------------------------------------------------------------------------------------------------------------------------------------------

ops_data_melt_KPI <- ops_data_KPI %>%
  select(
    Date,
    Trading.Party.ID,
    OPS,
    key,
    TaskCompletion,
    ops.mean.taskcompletion,
    TaskVolume,
    threshold.taskcompletion
  ) %>%
  melt(
    id.vars =
      c(
        "Date",
        "Trading.Party.ID",
        "OPS",
        "TaskVolume",
        "key"
      )
  ) %>%
  mutate(
    TaskVolume =
      if_else (
        variable %in%
          c("OPS_Mean", "OPS_Median", "TaskShare"), 0, as.double(TaskVolume)
      ),
    key = as.factor(key))

ops_data_melt_API <- ops_data_API %>%
  select(
    Date,
    Trading.Party.ID,
    OPS,
    key,
    OutstandingOntime,
    ops.mean.outstanding,
    TotalOutstanding,
    threshold.outstanding
  ) %>%
  melt(
    id.vars =
      c(
        "Date",
        "Trading.Party.ID",
        "OPS",
        "TotalOutstanding",
        "key"
      )
  ) %>%
  mutate(
    TotalOutstanding =
      if_else (
        variable %in%
          c("OPS_Mean", "OPS_Median", "TaskShare"), 0, as.double(TotalOutstanding)
      ),
    key = as.factor(key))%>%
  rename(
    "TaskVolume" = TotalOutstanding
  )

ops_data_melt <- rbind(ops_data_melt_KPI, ops_data_melt_API)%>%
  filter(!is.na(value))

# Importing and preparing OPS IPRP data --------------------------------------------------------------------------------------------------------------------------------------------------------------------

IPRP_plans_ops <- read.csv(paste(dir,"/data/inputs/IPRP_plans_ops.csv",sep="")) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), key = as.factor(paste(Trading.Party.ID, OPS, indicator)))

cols <- c("Trading.Party.ID", "OPS", "Batch", "key")

IPRP_plans_ops_data_KPI <- left_join(
  ops_data_KPI,
  IPRP_plans_ops,
  by = c(
    "Date",
    "Trading.Party.ID",
    "OPS",
    "key")
) %>%
  group_by(key) %>%
  mutate(Batch = max(Batch, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(cols, factor) %>%
  select(Date, Trading.Party.ID, OPS, key, Batch, TaskCompletion, TaskVolume, ops.mean.taskcompletion, threshold.taskcompletion, Planned_Perf)

IPRP_plans_ops_melt_KPI <- melt(
  IPRP_plans_ops_data_KPI,
  id.vars =
    c("Date", "Trading.Party.ID", "OPS", "TaskVolume", "Batch", "key"
    )
) %>%
  mutate(
    category = str_sub(as.character(Trading.Party.ID), -1, -1),
    TaskVolume = if_else(
      variable %in% c("OPS_Mean", "OPS_Median", "Planned_Perf", "TaskShare"), 0,
      as.double(TaskVolume)))


IPRP_plans_ops_data_API <- left_join(
  ops_data_API,
  IPRP_plans_ops,
  by = c(
    "Date",
    "Trading.Party.ID",
    "OPS",
    "key")
) %>%
  group_by(key) %>%
  mutate(Batch = max(Batch, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(cols, factor) %>%
  select(Date, Trading.Party.ID, OPS, key, Batch, OutstandingOntime, TotalOutstanding, ops.mean.outstanding, threshold.outstanding, Planned_Perf)

IPRP_plans_ops_melt_API <- melt(
  IPRP_plans_ops_data_API%>%rename("TaskCompletion" = OutstandingOntime),
  id.vars =
    c("Date", "Trading.Party.ID", "OPS", "TotalOutstanding", "Batch", "key"
    )
) %>%
  mutate(
    category = str_sub(as.character(Trading.Party.ID), -1, -1),
    TotalOutstanding = if_else(
      variable %in% c("OPS_Mean", "OPS_Median", "Planned_Perf", "TaskShare"), 0,
      as.double(TotalOutstanding)))%>%
  rename(
    "TaskVolume" = TotalOutstanding)

IPRP_plans_ops_melt <- rbind(IPRP_plans_ops_melt_KPI, IPRP_plans_ops_melt_API)%>%
  filter(!is.na(value))%>%
  mutate(variable = factor(variable,c("TaskCompletion", "ops.mean.taskcompletion",
                                      "ops.mean.outstanding", "threshold.taskcompletion",
                                      "threshold.outstanding", "Planned_Perf")))


# Importing OPS IPRP status sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

iprp_status_ops <- readRDS(paste(dir,"/data/rdata/IPRP_plan_comparison_ops.Rda", sep = "")) %>%
  mutate(
    Status = case_when(
      OnTrack == 1 ~ "On Track",
      Close == 1 ~ "Close",
      OffTrack==1 ~ "Off Track"),
    TaskCompletion = ifelse(indicator=="KPI",TaskCompletion, OutstandingOntime),
    TaskCompletion = as.numeric(format(TaskCompletion, digits = 1)),
    Planned_Perf = as.numeric(format(Planned_Perf, digits = 1)),
    key = paste(Trading.Party.ID, OPS, indicator),
    OPS = paste(OPS, indicator)
  ) %>%
  select(Trading.Party.ID, Date, OPS, Batch, TaskCompletion, Planned_Perf, Status,key)


# Importing performance tracking sheet --------------------------------------------------------------------------------------------------------------------------------------------------------------------

tracking_ops <- read.csv(paste(dir,"/data/inputs/tracking_ops.csv",sep="")) %>%
  rename("Trading.Party.ID" = ORG_ID) %>%
  mutate(
    Date = format(as.Date(Date, format = "%d/%m/%Y"), "%Y-%m"),
    Rationale = as.character(Rationale),
    PFM_Commentary = as.character(PFM_Commentary)
  )

tracking_ops_pfm_opsperf <- tracking_ops %>%
  filter(
    Category == "Performance_Trigger_3m_KPI" |
      Category == "Performance_Trigger_3m_API" |
      Category == "Performance_Trigger_6m_KPI" |
      Category == "Performance_Trigger_6m_API" |
      Category == "Watch_list",
    PFM_Commentary != "none",
    PFM_Commentary != "") %>%
  select(Trading.Party.ID, Date, OPS, PFM_Commentary)

tracking_ops_pfm_mile <- tracking_ops %>%
  filter(Category == "Milestone_Trigger",
         PFM_Commentary != "none",
         PFM_Commentary != "") %>%
  select(Trading.Party.ID, Date, OPS, PFM_Commentary)

tracking_ops_performance <- tracking_ops %>%
  filter(Category == "Performance_Trigger_3m_KPI" |
           Category == "Performance_Trigger_3m_API" |
           Category == "Performance_Trigger_6m_KPI" |
           Category == "Performance_Trigger_6m_API" |
           Category == "Watch_list") %>%
  mutate(
    OPS = paste(OPS, substr(Category, nchar(as.character(Category))-2, nchar(as.character(Category))))
  )%>%
  select(Trading.Party.ID, Date, OPS, Action, Rationale)

tracking_ops_milestone <- tracking_ops %>%
  filter(Category == "Milestone_Trigger") %>%
  select(Trading.Party.ID, Date, OPS, Batch, Action, Rationale)

tracking_ops_watch <- readRDS(paste(dir, "/data/rdata/monthly-tracking-ops.Rda", sep = ""))%>%
  filter(Category == "Watch_list") %>%
  select(Trading.Party.ID, OPS)

tracking_ops_requested <- tracking_ops %>%
  filter((Action == "Extend" | Action == "IPRP" | Action == "Resubmit") & Response_Received == "")

}
