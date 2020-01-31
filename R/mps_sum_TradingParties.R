#' Summarise Trading Parties by MPS
#'
#' @param my.dir character
#' @param mps_data_clean character
#'
#' @return
#' @export
#'
#' @examples

mps_sum_TradingParties <- function(
  my.dir = getwd(),
  df = readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda")),
  tp.details = utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
  ) {

  tp_details <- tp.details

  mps_sum_TradingParties <- df %>%
    dplyr::group_by(Date, Trading.Party.ID) %>%
    dplyr::summarise(
      Agg_Tasks = sum(TaskVolume, na.rm = TRUE),
      Agg_OnTime = sum(OnTimeTasks, na.rm = TRUE),
      Agg_Perf = Agg_OnTime / Agg_Tasks
      ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      tp_details,
      by = c("Trading.Party.ID")
      ) %>%
    dplyr::arrange(Trading.Party.ID, Date) %>%
    dplyr::group_by(Trading.Party.ID) %>%
    dplyr::mutate(
      Agg_Perf_roll = zoo::rollapply(Agg_Perf, 6, mean, align = "right", fill = NA)
      ) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(Agg_Perf_roll) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(
      n_all = dplyr::n(),
      rank_all = rank(-Agg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Date, tp_type) %>%
    dplyr::mutate(
      n_type = dplyr::n(),
      rank_type = rank(-Agg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Date, sub_type) %>%
    dplyr::mutate(
      n_subtype = dplyr::n(),
      rank_subtype = rank(-Agg_Perf_roll)
    ) %>%
    dplyr::ungroup()

  return(mps_sum_TradingParties)

}
