
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param dir character
#' @param ops.data character
#' @param ops.thresholds character
#' @param tp.details character
#' @param os.tasks.threshold numeric
#' @param filter.date character
#' @param csv.outputs character
#' @param rda.outputs character
#'
#' @return
#' @export
#'
#' @examples

ops_data_prep <- function(
  dir = getwd(),
  ops.data = paste0(dir, "/data/inputs/OPS_data.csv"),
  ops.thresholds = paste0(dir, "/data/inputs/ops_thresholds.csv"),
  tp.details = paste0(dir, "/data/inputs/tp_details.csv"),
  os.tasks.threshold = 0.75,
  filter.date = "2019-04-01",
  csv.outputs = paste0(dir, "/data/outputs"),
  rda.outputs = paste0(dir, "/data/rdata")
  ) {


# Importing raw data ------------------------------------------------------

  ops_data <- read.csv(ops.data)
  ops_thresholds <- read.csv(ops.thresholds)
  tp_details <- read.csv(paste0(dir, "/data/inputs/tp_details.csv"))
  outstanding.tasks.threshold <- os.tasks.threshold


# Cleaning and joining thresholds to ops data -----------------------------

  ops_data_clean <- ops_data %>%
    rename(
      "Trading.Party.ID" = Trading.Party.Name,
      "Date" = Period,
      "OPS" = Standard,
      "TaskVolume" = Tasks.Completed.Within.Period,
      "TotalOutstanding" = Tasks.Outstanding.End.Period
    ) %>%
    mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      TaskCompletion = Tasks.Completed.Within.Time / TaskVolume,
      OutstandingOntime = Tasks.Outstanding.Within.Time / TotalOutstanding
    ) %>%
    select(
      Date,
      Trading.Party.ID,
      OPS,
      TaskCompletion,
      TaskVolume,
      OutstandingOntime,
      TotalOutstanding,
      OutstandingOntime
    ) %>%
    filter(
      Date >= filter.date
    )

  ops_data_clean <- left_join(ops_data_clean, tp_details, by = c("Trading.Party.ID"))

  ops_data_clean <- left_join(ops_data_clean, ops_thresholds, by = c("OPS")) %>%
    mutate(
      OPS = factor(
        OPS,
        levels = c(
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b"))) %>%
    na.omit()


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    group_by(Date, OPS) %>%
    summarize(
      ops.mean.taskcompletion = mean(TaskCompletion, na.rm = TRUE),
      ops.mean.outstanding=mean(OutstandingOntime,na.rm = TRUE),
      OPS_Median = median(TaskCompletion, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume)
    ) %>%
    ungroup() %>%
    arrange(OPS, Date)

  write.csv(ops_summary, paste0(csv.outputs, "/ops_summary.csv"))
  saveRDS(ops_summary, file = paste0(rda.outputs,"/ops_summary.Rda"))


# Joining ops data with summary -------------------------------------------

  ops_data_clean <- left_join(ops_data_clean, ops_summary, by = c("Date", "OPS"))

  ops_data_clean <- ops_data_clean %>%
    mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      check1 = TaskCompletion < threshold,
      check2 = OutstandingOntime < outstanding.tasks.threshold,
      key = as.factor(paste(Trading.Party.ID, OPS))
    )

  write.csv(ops_data_clean, paste0(csv.outputs, "/OPS_data_clean.csv"))
  saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))

}
