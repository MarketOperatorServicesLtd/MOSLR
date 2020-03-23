
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param ops.data character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param my.dir character
#' @param save.output logical
#' @param ops.details character
#' @param ops.thresholds character
#' @param ops.charges character
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

ops_data_prep <- function(
  my.dir = getwd(),
  ops.data = utils::read.csv(paste0(my.dir, "/data/inputs/OPS_data.csv")),
  ops.details = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv")),
  ops.thresholds = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_thresholds.csv")),
  ops.charges = utils::read.csv(paste0(my.dir, "/data/inputs/OPS_Charges.csv")),
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  save.output = TRUE
  ) {


  # Cleaning data -----------------------------

  ops_data_clean <- ops.data %>%
    dplyr::rename(
      TaskVolumeCompleted = Tasks.Completed.Within.Period,
      TaskVolumeOutstanding = Tasks.Outstanding.End.Period,
      OnTimeTasksCompleted = Tasks.Completed.Within.Time,
      OnTimeTasksOutstanding = Tasks.Outstanding.Within.Time
      ) %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y"),
      TaskCompletion = OnTimeTasksCompleted / TaskVolumeCompleted,
      OutstandingOntime = OnTimeTasksOutstanding / TaskVolumeOutstanding
      ) %>%
    dplyr::filter(!Standard %in% c("OPS G4a", "OPS G4b"))

  tasks_completed <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID, Period, Standard,
      TaskCompletion, TaskVolumeCompleted
      ) %>%
    dplyr::rename(
      Performance = TaskCompletion,
      TaskVolume = TaskVolumeCompleted
      ) %>%
    dplyr::mutate(
      PerformanceMeasure = "Completed"
      )

  tasks_outstanding <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID, Period, Standard,
      OutstandingOntime, TaskVolumeOutstanding
      ) %>%
    dplyr::rename(
      Performance = OutstandingOntime,
      TaskVolume = TaskVolumeOutstanding
      ) %>%
    dplyr::mutate(
      PerformanceMeasure = "Outstanding"
      )

  ops_data_clean <- base::rbind(tasks_completed, tasks_outstanding)

  ops_thresholds <- ops.thresholds %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y")
      )

  ops.charges <- ops.charges %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y")
      )


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    dplyr::group_by(Period, Standard, PerformanceMeasure) %>%
    dplyr::summarise(
      MarketMean = mean(Performance, na.rm = TRUE),
      MarketMedian = median(Performance, na.rm = TRUE),
      MarketTaskVolume = sum(TaskVolume, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Standard, Period)

  ops_data_clean <-
    dplyr::left_join(
      ops_data_clean,
      ops_summary,
      by = c("Period", "Standard", "PerformanceMeasure")
      ) %>%
    dplyr::left_join(
      ops_thresholds,
      by = c("Standard", "Period", "PerformanceMeasure")
      ) %>%
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
    dplyr::mutate(
      Threshold = zoo::na.locf(Threshold, na.rm = FALSE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      ops.charges,
      by = c("Trading.Party.ID", "Period", "Standard")
      ) %>%
    dplyr::left_join(
      ops.details,
      by = c("Standard")
      ) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / MarketTaskVolume,
      BelowPeer = dplyr::if_else (
        !is.na(Threshold) > 0,
        Performance < Threshold,
        Performance < MarketMean
        ),
      Standard = factor(
        Standard,
        levels = c(
          "OPS A1a", "OPS A2a", "OPS A2b", "OPS A2c", "OPS A3a",
          "OPS A3b", "OPS A4a",
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b")
        )
      )


  if (save.output) {
    saveRDS(ops_summary, file = paste0(rda.outputs, "/ops_summary.Rda"))
    utils::write.csv(ops_data_clean, paste0(csv.outputs, "/ops_data_clean.csv"))
    saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))
  }

  invisible(ops_data_clean)

}
