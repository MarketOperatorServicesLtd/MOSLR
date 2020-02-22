#' Aggregates Trading Party OPS Performance
#'
#' @param df dataframe
#' @param tp.details dataframe
#' @param my.dir character
#'
#' @return
#' @export
#'
#' @examples

ops_aggregate_perf <- function(
  my.dir = getwd(),
  df = readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda")),
  tp.details = utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
) {

  ops_aggregate_perf <- df %>%
    dplyr::group_by(PerformanceMeasure, Trading.Party.ID, Period) %>%
    dplyr::summarise(
      Agg_Perf = stats::weighted.mean(Performance, TaskVolume, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      tp.details,
      by = c("Trading.Party.ID")
      ) %>%
    dplyr::arrange(PerformanceMeasure, Trading.Party.ID, Period) %>%
    dplyr::group_by(PerformanceMeasure, Trading.Party.ID) %>%
    dplyr::mutate(
      Agg_Perf_roll = zoo::rollapply(Agg_Perf, 6, mean, align = "right", fill = NA)
      ) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(Agg_Perf_roll) %>%
    dplyr::group_by(PerformanceMeasure, Period) %>%
    dplyr::mutate(
      n_all = dplyr::n(),
      rank_all = rank(-Agg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PerformanceMeasure, tp_type, Period) %>%
    dplyr::mutate(
      n_type = dplyr::n(),
      rank_type = rank(-Agg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PerformanceMeasure, sub_type, Period) %>%
    dplyr::mutate(
      n_subtype = dplyr::n(),
      rank_subtype = rank(-Agg_Perf_roll)
      ) %>%
    dplyr::ungroup()

  return(ops_aggregate_perf)

}
