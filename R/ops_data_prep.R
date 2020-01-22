
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param dir character
#' @param ops.data character
#' @param tp.details character
#' @param filter.date character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param ops.thresholds.kpi character
#' @param ops.thresholds.api character
#'
#' @return
#' @export
#'
#' @importFrom dplyr rename mutate select filter left_join group_by summarize ungroup arrange
#' @importFrom stats median
#' @importFrom utils read.csv write.csv
#'
#' @examples

ops_data_prep <- function(
  dir = getwd(),
  ops.data = paste0(dir, "/data/inputs/OPS_data.csv"),
  ops.thresholds.kpi = paste0(dir, "/data/inputs/ops_thresholds_kpi.csv"),
  ops.thresholds.api = paste0(dir, "/data/inputs/ops_thresholds_api.csv"),
  tp.details = paste0(dir, "/data/inputs/tp_details.csv"),
  filter.date = "2019-04-01",
  csv.outputs = paste0(dir, "/data/outputs"),
  rda.outputs = paste0(dir, "/data/rdata")
  ) {


# Importing raw data ------------------------------------------------------

  ops_data <- utils::read.csv(ops.data)
  ops_thresholds_kpi <- utils::read.csv(ops.thresholds.kpi)
  ops_thresholds_api <- utils::read.csv(ops.thresholds.api)
  tp_details <- utils::read.csv(paste0(dir, "/data/inputs/tp_details.csv"))


# Cleaning and joining thresholds to ops data -----------------------------

  ops_data_clean <- ops_data %>%
    dplyr::rename(
      "Trading.Party.ID" = Trading.Party.Name,
      "Date" = Period,
      "OPS" = Standard,
      "TaskVolume" = Tasks.Completed.Within.Period,
      "TotalOutstanding" = Tasks.Outstanding.End.Period
    ) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      TaskCompletion = Tasks.Completed.Within.Time / TaskVolume,
      OutstandingOntime = Tasks.Outstanding.Within.Time / TotalOutstanding
    ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      OPS,
      TaskCompletion,
      TaskVolume,
      OutstandingOntime,
      TotalOutstanding,
      OutstandingOntime
    ) %>%
    dplyr::filter(
      Date >= filter.date
    ) %>%
    dplyr::left_join(
      tp_details,
      by = c("Trading.Party.ID")
      ) %>%
    dplyr::left_join(
      ops_thresholds_kpi,
      by = c("OPS")
      ) %>%
    dplyr::left_join(
      ops_thresholds_api,
      by = c("OPS")
    ) %>%
    dplyr::mutate(
      OPS = factor(
        OPS,
        levels = c(
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b")
        )
      ) %>%
    stats::na.omit()


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    dplyr::group_by(Date, OPS) %>%
    dplyr::summarize(
      ops.mean.taskcompletion = mean(TaskCompletion, na.rm = TRUE),
      ops.mean.outstanding = mean(OutstandingOntime,na.rm = TRUE),
      OPS_Median = median(TaskCompletion, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(OPS, Date)

  utils::write.csv(ops_summary, paste0(csv.outputs, "/ops_summary.csv"))
  saveRDS(ops_summary, file = paste0(rda.outputs,"/ops_summary.Rda"))


# Joining ops data with summary -------------------------------------------

  ops_data_clean <- dplyr::left_join(
    ops_data_clean,
    ops_summary,
    by = c("Date", "OPS")
    ) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      check1 = TaskCompletion < kpi_threshold,
      check2 = OutstandingOntime < api_threshold,
      key = as.factor(paste(Trading.Party.ID, OPS))
    )

  utils::write.csv(ops_data_clean, paste0(csv.outputs, "/OPS_data_clean.csv"))
  saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))

}
