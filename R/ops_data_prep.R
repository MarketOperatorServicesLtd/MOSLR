
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param ops.data character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param my.dir character
#' @param save.output boolean
#' @param ops.details character
#' @param ops.thresholds character
#' @param DataBase boolean
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
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  save.output = TRUE,
  DataBase = TRUE
  ) {




  if(DataBase) {

    con <- odbc::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "data-mgmt",
                         Database = "MOSL_Sandpit",
                         Port = 1433,
                         trusted_connection = "True")

    ops.data <- MOSLR::data_copy()
    ops.details <- dplyr::tbl(con, "PERF_StandardsDetails") %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        Details = iconv(Details),
        Context = iconv(Context)
        )
    ops.thresholds <- dplyr::tbl(con, "PERF_StandardsThresholds") %>% dplyr::as_tibble()

  } else {
      ops.data <- utils::read.csv(paste0(my.dir, "/data/inputs/OPS_data.csv"))
      ops.details = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv"))
      ops.thresholds = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_thresholds.csv"))
    }


  # Cleaning data -----------------------------

  ops_data_clean <- ops.data %>%
    dplyr::rename(
      TaskVolumeCompleted = TasksCompletedWithinPeriod,
      TaskVolumeOutstanding = TasksOutstandingEndPeriod,
      OnTimeTasksCompleted = TasksCompletedWithinTime,
      OnTimeTasksOutstanding = TasksOutstandingWithinTime
      ) %>%
    dplyr::mutate(
      Period = as.Date(Period),
      TaskCompletion = OnTimeTasksCompleted / TaskVolumeCompleted,
      OutstandingOntime = OnTimeTasksOutstanding / TaskVolumeOutstanding
      ) %>%
    dplyr::mutate_at(dplyr::vars(TaskCompletion, OutstandingOntime), ~replace(., is.nan(.), NA)) %>%
    dplyr::filter(!Standard %in% c(
      "OPS G4a",
      "OPS G4b",
      "OPS A1a",
      "OPS A2a",
      "OPS A2b",
      "OPS A2c",
      "OPS A3a",
      "OPS A3b",
      "OPS A4a"
      )
      )

  tasks_completed <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID,
      Period,
      Standard,
      TaskCompletion,
      TaskVolumeCompleted,
      OnTimeTasksCompleted
      ) %>%
    dplyr::rename(
      Performance = TaskCompletion,
      TaskVolume = TaskVolumeCompleted,
      OnTimeTasks = OnTimeTasksCompleted
      ) %>%
    dplyr::mutate(PerformanceMeasure = "Completed",
      Charges = (TaskVolume - OnTimeTasks) * 40
      )

  tasks_outstanding <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID,
      Period,
      Standard,
      OutstandingOntime,
      TaskVolumeOutstanding,
      OnTimeTasksOutstanding
      ) %>%
    dplyr::rename(
      Performance = OutstandingOntime,
      TaskVolume = TaskVolumeOutstanding,
      OnTimeTasks = OnTimeTasksOutstanding
      ) %>%
    dplyr::mutate(
      PerformanceMeasure = "Outstanding",
      Charges = 0
      )

  ops_data_clean <- base::rbind(tasks_completed, tasks_outstanding)

  ops_thresholds <- ops.thresholds %>%
    dplyr::mutate(Period = as.Date(Period, format = "%d/%m/%Y"))


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    dplyr::filter(!is.na(Performance)) %>%
    dplyr::group_by(Period, Standard, PerformanceMeasure) %>%
    dplyr::summarise(
      MarketMean = mean(Performance, na.rm = TRUE),
      MarketMedian = median(Performance, na.rm = TRUE),
      MarketTaskVolume = sum(TaskVolume, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Standard, Period)

  ops_data_clean <- ops_data_clean %>%
    dplyr::left_join(ops_summary, by = c("Period", "Standard", "PerformanceMeasure")) %>%
    dplyr::left_join(ops_thresholds, by = c("Standard", "Period", "PerformanceMeasure")) %>%
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
    dplyr::mutate(Threshold = zoo::na.locf(Threshold, na.rm = FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ops.details, by = c("Standard")) %>%
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

    if(DataBase){
      odbc::dbWriteTable(con, "PERF_OPSSummary", ops_summary, overwrite = TRUE)
      odbc::dbWriteTable(con, "PERF_OPSDataClean", ops_data_clean, overwrite = TRUE)
    } else{
       saveRDS(ops_summary, file = paste0(rda.outputs, "/ops_summary.Rda"))
    utils::write.csv(ops_data_clean, paste0(csv.outputs, "/ops_data_clean.csv"))
    saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))
    }
  }

  if(DataBase) odbc::dbDisconnect(con)
  invisible(ops_data_clean)

}
