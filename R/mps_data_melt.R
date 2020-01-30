#' Melt MPS Data
#'
#' This function pivots the the MPS data
#' so that performance can be plotted in a
#' graph.
#'
#' @param mps.data file
#' @param filter.date.from date
#' @param iprp.plans file
#' @param filter.date.to date
#' @param save.output logical
#' @param save.dir character
#' @param my.dir character
#'
#' @return
#' @export
#'
#' @examples

melt_mps_data <- function(
  my.dir = getwd(),
  mps.data = readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda")),
  filter.date.from = "2018-04-01",
  filter.date.to = Sys.Date(),
  iprp.plans = utils::read.csv(paste0(my.dir, "/data/inputs/IPRP_plans_mps.csv")),
  save.output = TRUE,
  save.dir = paste0(my.dir, "/data/rdata/mps_data_melt.Rda")
  ) {

  # Importing data ----------------------------------------------------------

  mps_data_clean <- mps.data

  IPRP_plans_mps <- iprp.plans %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
    )

  # Melting MPS and IPRP data join for graphs ---------------------------------------------

  mps_data_melt <- mps_data_clean %>%
    dplyr::filter(
      Date >= filter.date.from,
      Date <= filter.date.to) %>%
    dplyr::left_join(
      IPRP_plans_mps, by = c("Date", "Trading.Party.ID", "MPS", "key")
      ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare,
      Planned_Perf
      ) %>%
    dplyr::filter(TaskVolume > 0) %>%
    tidyr::gather(
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare, Planned_Perf
      ) %>%
    dplyr::mutate(
      TaskVolume =
        dplyr::if_else (
          variable %in%
            c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"),
          0, as.double(TaskVolume)
        ),
      key = as.factor(key),
      variable = factor(variable, levels = c("TaskCompletion", "MPS_Mean", "MPS_Median", "TaskShare", "Planned_Perf"))
    )

  if (save.output) {
  saveRDS(mps_data_melt, save.dir)
  }

  invisible(mps_data_melt)

}
