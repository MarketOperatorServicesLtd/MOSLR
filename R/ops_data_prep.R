
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param ops.data character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param my.dir character
#' @param save.output logical
#'
#' @return
#' @export
#'
#'
#' @examples

ops_data_prep <- function(
  my.dir = getwd(),
  ops.data = utils::read.csv(paste0(my.dir, "/data/inputs/OPS_data.csv")),
  ops.thresholds = utils::read.csv(paste0(my.dir, "/data/inputs/ops_thresholds.csv")),
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
      Date = as.Date(Date, format = "%d/%m/%Y"),
      TaskCompletion = OnTimeTasksCompleted / TaskVolumeCompleted,
      OutstandingOntime = OnTimeTasksOutstanding / TaskVolumeOutstanding
      )

  tasks_completed <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID, Date, OPS,
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
      Trading.Party.ID, Date, OPS,
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


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    dplyr::group_by(Date, OPS, PerformanceMeasure) %>%
    dplyr::summarise(
      OPS_Mean = mean(Performance, na.rm = TRUE),
      OPS_Median = median(Performance, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(OPS, Date)

  ops_data_clean <-
    dplyr::left_join(
      ops_data_clean,
      ops_summary,
      by = c("Date", "OPS", "PerformanceMeasure")
      ) %>%
    dplyr::left_join(
      ops.thresholds,
      by = c("OPS", "PerformanceMeasure")
      ) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      BelowPeer = dplyr::if_else (
        Threshold > 0,
        Performance < Threshold,
        Performance < OPS_Mean
        ),
      OPS = factor(
        OPS,
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
  utils::write.csv(ops_summary, paste0(csv.outputs, "/ops_summary.csv"))
  utils::write.csv(ops_data_clean, paste0(csv.outputs, "/OPS_data_clean.csv"))
  saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))
  }

  invisible(ops_data_clean)

}
