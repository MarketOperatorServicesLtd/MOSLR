#' Aggregates Trading Party OPS Performance
#'
#' @param df dataframe
#' @param tp.details dataframe
#' @param my.dir character
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

ops_aggregate_perf <- function(
  my.dir = getwd(),
  df = readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda")),
  tp.details = utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv")),
  spid.counts = utils::read.csv(paste0(my.dir, "/data/inputs/spid_counts.csv"))
) {

  ops_aggregate_perf <- df %>%
    dplyr::group_by(PerformanceMeasure, Period, Trading.Party.ID) %>%
    dplyr::summarise(
      Avg_Perf = stats::weighted.mean(Performance, TaskVolume, na.rm = TRUE),
      TaskVolume = sum(TaskVolume, na.rm = TRUE),
      OnTimeTasks = sum(OnTimeTasks, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(tp.details, by = c("Trading.Party.ID")) %>%
    dplyr::left_join(spid.counts, by = c("Trading.Party.ID" = "TradingPartyID")) %>%
    dplyr::arrange(PerformanceMeasure, Trading.Party.ID, Period) %>%
    dplyr::group_by(PerformanceMeasure, Trading.Party.ID) %>%
    dplyr::mutate(
      Avg_Perf_roll = zoo::rollapply(Avg_Perf, 6, mean, align = "right", fill = NA),
      Agg_TaskV_roll = zoo::rollapply(TaskVolume, 6, sum, align = "right", fill = NA),
      Agg_Comp_roll = zoo::rollapply(OnTimeTasks, 6, sum, align = "right", fill = NA),
      Agg_Perf_roll = Agg_Comp_roll / Agg_TaskV_roll
      ) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(Avg_Perf_roll) %>%
    dplyr::group_by(PerformanceMeasure, Period) %>%
    dplyr::mutate(
      n_all = dplyr::n(),
      rank_all = rank(-Avg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PerformanceMeasure, tp_type, Period) %>%
    dplyr::mutate(
      n_type = dplyr::n(),
      rank_type = rank(-Avg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PerformanceMeasure, sub_type, Period) %>%
    dplyr::mutate(
      n_subtype = dplyr::n(),
      rank_subtype = rank(-Avg_Perf_roll)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      LeagueGroup = dplyr::case_when(
        sub_type == "NAV" & tp_type == "Wholesaler" ~ "NAV Wholesalers",
        sub_type == "Self-supply" | sub_type == "NAV" ~ "Self Supply Retailers & NAVs",
        sub_type == "WaSC" ~ "Wholesalers (WaSCs)",
        sub_type == "WoC" ~ "Wholesalers (WoCs)",
        TotalSPID >= 5000 ~ "Retailers with > 5k SPIDs",
        TotalSPID < 5000 ~ "Retailers with < 5k SPIDs",
        TRUE ~ "Other"
      ),
      LeagueGroup = factor(
        LeagueGroup,
        levels = c(
          "Retailers with > 5k SPIDs",
          "Retailers with < 5k SPIDs",
          "Self Supply Retailers & NAVs",
          "Wholesalers (WaSCs)",
          "Wholesalers (WoCs)",
          "NAV Wholesalers"
        )
      )
    ) %>%
    dplyr::group_by(PerformanceMeasure, Period, LeagueGroup) %>%
    dplyr::mutate(
      n_league = dplyr::n(),
      rank_league = rank(-Agg_Perf_roll)
    ) %>%
    dplyr::ungroup()

  return(ops_aggregate_perf)

}
