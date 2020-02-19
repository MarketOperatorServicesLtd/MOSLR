
#' MPS Data Preparation
#'
#'Prepares the raw MPS data so it is ready to be processed.
#'
#' @param my.dir character
#' @param mps.data character
#' @param mps.thresholds character
#' @param csv.outputs character
#' @param rda.outputs character
#'
#' @return
#' @export
#'
#' @importFrom stats median
#'
#' @examples

mps_data_prep <- function(
  my.dir = getwd(),
  mps.data = utils::read.csv(paste0(my.dir, "/data/inputs/MPS_data.csv")),
  mps.thresholds = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_thresholds.csv")),
  tp.details = utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv")),
  Standards.details = utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv")),
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  save.output = TRUE
  ) {


  # Importing raw data ------------------------------------------------------

  mps_data <- mps.data
  mps_thresholds <- mps.thresholds %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y")
      )


  # Importing and cleaning MPS data -----------------------------------------

  mps_data_clean <- mps_data %>%
    dplyr::rename(
      Charges = Total.Performance.Charge.Value,
      TaskVolume = Total.number.of.tasks.compeleted.within.Period,
      OnTimeTasks = Number.of.tasks.completed.on.time
      ) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Performance = OnTimeTasks / TaskVolume,
      PerformanceMeasure = "Completed"
      ) %>%
    dplyr::arrange(
      Date, Trading.Party.ID, Standard
      ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      Standard,
      PerformanceMeasure,
      Performance,
      TaskVolume,
      Charges,
      OnTimeTasks
      ) %>%
    dplyr::left_join(
      Standards.details,
      by = c("Standard")
      ) %>%
    dplyr::left_join(
      mps_thresholds,
      by = c("Standard", "Date", "PerformanceMeasure")
      ) %>%
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
    dplyr::mutate(
      Threshold = zoo::na.locf(Threshold, na.rm = FALSE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Standard = factor(
        Standard,
        levels = c(
          "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
          "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
          "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19"
        )),
      key = as.factor(paste(Trading.Party.ID, Standard))
      )


  # Creating summary grouped by Standard with market metrics by month ------------

  mps_summary <- mps_data_clean %>%
    dplyr::group_by(Date, Standard) %>%
    dplyr::summarise(
      MarketMean = mean(Performance, na.rm = TRUE),
      MarketMedian = median(Performance, na.rm = TRUE),
      MarketTaskVolume = sum(TaskVolume)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Standard, Date)

  mps_data_clean <-
    dplyr::left_join(
      mps_summary,
      mps_data_clean,
      by = c("Date", "Standard")
      ) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / MarketTaskVolume,
      BelowPeer = dplyr::if_else(
        Threshold > 0,
        Performance < Threshold,
        Performance < MarketMean
        )
      )

  if (save.output) {
    saveRDS(mps_summary, file = paste0(rda.outputs, "/mps_summary.Rda"))
    utils::write.csv(mps_data_clean, paste0(csv.outputs, "/MPS_data_clean.csv"))
    saveRDS(mps_data_clean, file = paste0(rda.outputs, "/mps_data_clean.Rda"))
    }

  invisible(mps_data_clean)

}
