
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
#' @importFrom dplyr rename mutate select filter left_join group_by summarise ungroup
#' @importFrom stats median
#' @importFrom utils utils::read.csv utils::write.csv
#'
#' @examples

mps_data_prep <- function(
  my.dir = getwd(),
  mps.data = paste0(my.dir, "/data/inputs/MPS_data.csv"),
  mps.thresholds = paste0(my.dir, "/data/inputs/mps_thresholds.csv"),
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata")
  ) {

  # Importing raw data ------------------------------------------------------


  mps_data <- utils::read.csv(mps.data)
  mps_thresholds <- utils::read.csv(mps.thresholds)


  # Importing and cleaning MPS data -----------------------------------------

  mps_data_clean <- mps_data %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      TaskCompletion = (Number.of.tasks.completed.on.time / Total.number.of.tasks.compeleted.within.Period)
    ) %>%
    dplyr::rename(
      Charges = Total.Performance.Charge.Value,
      TaskVolume = Total.number.of.tasks.compeleted.within.Period,
      MPS = Market.Performance.Standard.No.
    ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      MPS,
      TaskCompletion,
      TaskVolume,
      Charges
    )

  mps_data_clean <- mps_data_clean %>%
    dplyr::left_join(
      mps_thresholds,
      by = c("MPS")
      ) %>%
    dplyr::mutate(
      MPS = factor(
        MPS,
        levels = c(
          "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
          "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
          "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19"
        ))
      )


  # Creating summary grouped by MPS with market metrics by month ------------

  mps_summary <- mps_data_clean %>%
    dplyr::group_by(Date, MPS) %>%
    dplyr::summarise(
      MPS_Mean = mean(TaskCompletion, na.rm = TRUE),
      MPS_Median = median(TaskCompletion, na.rm = TRUE),
      TotalTaskVolume = sum(TaskVolume)
    ) %>%
    dplyr::arrange(MPS, Date) %>%
    dplyr::ungroup()

  utils::write.csv(mps_summary, file = paste0(csv.outputs, "/mps_summary.csv"))
  saveRDS(mps_summary, file = paste0(rda.outputs, "/mps_summary.Rda"))


  mps_data_clean <- dplyr::left_join(mps_summary, mps_data_clean, by = c("Date", "MPS")) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / TotalTaskVolume,
      BelowPeer = if_else (
        mps_threshold > 0,
        TaskCompletion < mps_threshold,
        TaskCompletion < MPS_Mean
        ),
      key = as.factor(paste(Trading.Party.ID, MPS))
    )

  utils::write.csv(mps_data_clean, paste0(csv.outputs, "/MPS_data_clean.csv"))
  saveRDS(mps_data_clean, file = paste0(rda.outputs, "/mps_data_clean.Rda"))

  invisible(mps_data_clean)

}
