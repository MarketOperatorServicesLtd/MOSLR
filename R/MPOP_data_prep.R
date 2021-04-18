
#' Prepare MPOP report data
#'
#' Prepares data for bubble charts and/or Trading
#' Party ranking tables in MPOP reporting
#'
#'
#' @param df dataframe
#' @param df dataframe
#' @param df.tp character
#' @param output.table character
#' @param by character
#'
#' @return
#' @export
#'
#' @examples
#'

MPOP_data_prep <- function(
  df = NULL,
  df.tp = NULL,
  output.table = c("vacancy", "longunread"),
  by = "Retailer"
  ) {

  if (!by %in% c("Retailer", "Wholesaler", "Pair")) {
    stop("Variable 'by' in bubble_chart has to be one of c('Retailer', 'Wholesaler', 'Pair')")
  }

  if (output.table == "vacancy") {

    # Prep vacancy data -------------------------------------------------------

    last12_vac <- df %>%
      dplyr::filter(Period == max(df$Period, na.rm = TRUE) %m-% months(12)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(
        april_percent = sum(VacantPremises, na.rm = TRUE) / sum(Premises, na.rm = TRUE),
        Total_Premises_april = sum(Premises, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    last1_vac <- df %>%
      dplyr::filter(Period == max(df$Period) %m-% months(1)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(last_percent = sum(VacantPremises, na.rm = TRUE) / sum(Premises, na.rm = TRUE)) %>%
      dplyr::ungroup()

    if (by %in% c("Retailer", "Wholesaler")) {

      main_vac <- df.tp %>%
        dplyr::filter(
          Period == max(df$Period, na.rm = TRUE),
          stringr::str_sub(TradingPartyID, -2) == paste0("-", substr(by, 1, 1)),
          Premises > 200
          ) %>%
        dplyr::rename_at(dplyr::vars(TradingPartyName), dplyr::funs(paste0(by))) %>%
        dplyr::rename(
          TotalPremises = Premises,
          VacantPremises = Vacant_Premises) %>%
        dplyr::mutate(percent = VacantPremises / TotalPremises)

    } else {

      main_vac <- df %>%
        dplyr::filter(Period == max(df$Period)) %>%
        dplyr::group_by(Pair) %>%
        dplyr::summarise(
          percent = sum(VacantPremises) / sum(Premises),
          TotalPremises = sum(Premises),
          VacantPremises = sum(VacantPremises)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(TotalPremises > 6000) %>%
        dplyr::mutate(ShortName = Pair)

    }

    main_table <- main_vac %>%
      dplyr::left_join(last12_vac, by = by) %>%
      dplyr::mutate(
        change = percent - april_percent,
        "Change in size" = factor(
          dplyr::case_when(
            (TotalPremises - Total_Premises_april) / Total_Premises_april > 0.2 ~ "Number of premises increased by 20%",
            (TotalPremises - Total_Premises_april) / Total_Premises_april < 0.2 &
              (TotalPremises - Total_Premises_april) / Total_Premises_april > -0.2 ~ "Number of premises within +/- 20%",
            (TotalPremises - Total_Premises_april) / Total_Premises_april < -0.2 ~ "Number of premises reduced by > 20%",
            TRUE ~ "other"
            ),
          levels = c(
            "Number of premises increased by 20%",
            "Number of premises within +/- 20%",
            "Number of premises reduced by > 20%"
            )
          )
        ) %>%
      dplyr::arrange(desc(TotalPremises)) %>%
      dplyr::filter(!is.na(Total_Premises_april)) %>%
      dplyr::left_join(last1_vac, by = by)

  } else if (output.table == "longunread") {

    # Prep longunread data ----------------------------------------------------

    last12_long <- longunread %>%
      dplyr::filter(Period == max(longunread$Period) %m-% months(12)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(
        april_percent = sum(Meters_Unread_in_12mo) / sum(TotalMeters),
        Total_Meters_april = sum(TotalMeters)
        ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Total_Meters_april > 100) %>%
      dplyr::select(by, Total_Meters_april, april_percent)

    last1_long <- longunread %>%
      dplyr::filter(Period == max(longunread$Period) %m-% months(1)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(last_percent = sum(Meters_Unread_in_12mo) / sum(TotalMeters)) %>%
      dplyr::ungroup() %>%
      dplyr::select(by, last_percent)

    main_table <- longunread %>%
      dplyr::filter(Period == max(longunread$Period),!sub_type.x %in% c("NAV", "Self-supply")) %>%
      {
        if (by == "Retailer") {
          dplyr::group_by(., Retailer, ShortRet)
        } else if (by == "Wholesaler") {
          dplyr::group_by(., Wholesaler, ShortWhole)
        } else {
          dplyr::group_by(., Pair)
        }
      } %>%
      dplyr::summarise(
        percent = sum(Meters_Unread_in_12mo) / sum(TotalMeters),
        TotalMeters = sum(TotalMeters),
        PureLUMs = sum(Meters_Unread_in_12mo)
        ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(., last12_long, by = by) %>%
      dplyr::mutate(
        change = percent - april_percent,
        "Change in size" = dplyr::case_when(
          (TotalMeters - Total_Meters_april) / Total_Meters_april > 0.2 ~ "Meter numbers increased by 20%",
          (TotalMeters - Total_Meters_april) / Total_Meters_april < 0.2 &
            (TotalMeters - Total_Meters_april) / Total_Meters_april > -0.2 ~ "Meter number within +/- 20%",
          (TotalMeters - Total_Meters_april) / Total_Meters_april < -0.2 ~ "Meter numbers reduced by > 20%",
          TRUE ~ "other"
          )
        ) %>%
      dplyr::arrange(desc(TotalMeters)) %>%
      dplyr::filter(!is.na(Total_Meters_april)) %>%
      dplyr::left_join(., last1_long,
                       by = by) %>% {
                         if (by == "Retailer") {
                           dplyr::rename(., ShortName = ShortRet)
                         } else if (by == "Wholesaler") {
                           dplyr::rename(., ShortName = ShortWhole)
                         } else{
                           dplyr::mutate(., ShortName = Pair) %>%
                             dplyr::filter(., TotalMeters > 3000)
                         }
                       }

  }
  return(main_table)
}
