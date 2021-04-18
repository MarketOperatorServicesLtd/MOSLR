
#' Prepare MPOP report data
#'
#' Prepares data for bubble charts and/or Trading
#' Party ranking tables in MPOP reporting
#'
#'
#' @param df.longunread dataframe
#' @param df.vacancy dataframe
#' @param tp.details dataframe
#' @param longunread.table boolean
#' @param vacancy.table boolean
#' @param by character
#' @param my.dir character
#' @param load.data boolean
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'

MPOP_data_prep <- function(
  df.longunread = NULL,
  df.vacancy = NULL,
  tp.details = NULL,
  longunread.table = FALSE,
  vacancy.table = FALSE,
  by = "Retailer",
  my.dir = getwd(),
  load.data = FALSE
  ) {

  if (!by %in% c("Retailer", "Wholesaler", "Pair")) {
    stop("Variable 'by' in bubble_chart has to be one of c('Retailer', 'Wholesaler', 'Pair')")
  }

  my_dir <- my.dir
  longunread <- df.longunread
  vacant <- df.vacancy
  tp_details <- tp.details


  if (vacancy.table) {

    if (load.data) {

      tp_details <-
        readr::read_csv(paste0(my_dir, "/data/inputs/tp_details.csv")) %>%
        dplyr::mutate(
          TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
          ShortName = stringr::str_replace_all(ShortName, "&", "And")
        )

      vacant <-
        readr::read_csv(paste0(my_dir, "/data/inputs/Vacancy_Pairing.csv")) %>%
        dplyr::group_by(WholesalerID, Period) %>%
        dplyr::mutate(whole_vacant = sum(Premises)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(RetailerID, Period) %>%
        dplyr::mutate(ret_vacant = sum(Premises)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(
          whole_vacant > 200,
          ret_vacant > 200
          ) %>%
        #dplyr::mutate(Period = as.Date(Period)) %>%
        dplyr::left_join(tp_details, by = c("RetailerID" = "Trading.Party.ID")) %>%
        dplyr::rename(Retailer = TradingPartyName, ShortRet = ShortName) %>%
        dplyr::left_join(tp_details, by = c("WholesalerID" = "Trading.Party.ID")) %>%
        dplyr::rename(
          Wholesaler = TradingPartyName,
          ShortWhole = ShortName
          ) %>%
        dplyr::mutate(
          Pair = stringr::str_replace_all(paste(ShortRet, "+", ShortWhole), "&", "And"),
          Retailer = stringr::str_replace_all(Retailer, "&", "And"),
          Wholesaler = stringr::str_replace_all(Wholesaler, "&", "And")
        )

      vacant_TP <-
        readr::read_csv(paste0(my_dir, "/data/inputs/Vacancy_TP.csv")) %>%
        dplyr::select(TradingPartyID, Period, Premises, Vacant_Premises)

      vacant_Total <-
        readr::read_csv(paste0(my_dir, "/data/inputs/Vacancy_Total.csv"))

    } else {

      vacant_TP <- readr::read_csv(paste0(my_dir, "/data/inputs/Vacancy_TP.csv"))

      vacant_Total <- readr::read_csv(paste0(my_dir, "/data/inputs/Vacancy_Total.csv"))

    }


    # Prep vacancy data -------------------------------------------------------

    last12_vac <- vacant %>%
      dplyr::filter(Period == max(vacant$Period, na.rm = TRUE) %m-% months(12)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(
        april_percent = sum(VacantPremises, na.rm = TRUE) / sum(Premises, na.rm = TRUE),
        Total_Premises_april = sum(Premises, na.rm = TRUE)
      )

    last1_vac <- vacant %>%
      dplyr::filter(Period == max(vacant$Period) %m-% months(1)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(last_percent = sum(VacantPremises, na.rm = TRUE) / sum(Premises, na.rm = TRUE))

    if (by %in% c("Retailer", "Wholesaler")) {

      main_vac <- vacant_TP %>%
        dplyr::filter(
          Period == max(vacant$Period, na.rm = TRUE),
          stringr::str_sub(TradingPartyID, -2) == paste0("-", substr(by, 1, 1)),
          Premises > 200
          ) %>%
        dplyr::rename_at(dplyr::vars(TradingPartyName), dplyr::funs(paste0(by))) %>%
        dplyr::rename(
          TotalPremises = Premises,
          VacantPremises = Vacant_Premises) %>%
        dplyr::mutate(percent = VacantPremises / TotalPremises)

    } else {

      main_vac <- vacant %>%
        dplyr::filter(Period == max(vacant$Period)) %>%
        dplyr::group_by(Pair) %>%
        dplyr::summarise(
          percent = sum(VacantPremises) / sum(Premises),
          TotalPremises = sum(Premises),
          VacantPremises = sum(VacantPremises)
        ) %>%
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



  } else if (longunread.table) {

    if (load.data) {
      tp_details <-
        readr::read_csv(paste0(my_dir, "/data/inputs/tp_details.csv")) %>%
        dplyr::mutate(
          TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
          ShortName = stringr::str_replace_all(ShortName, "&", "And")
        )

      longunread <-
        readr::read_csv(paste0(my_dir, "/data/inputs/longunread.csv")) %>%
        dplyr::mutate(Period = as.Date(paste0(Period, "-01"), "%Y-%m-%d")) %>%
        dplyr::group_by(WholesalerID, Period) %>%
        dplyr::mutate(whole_meters = sum(TotalMeters)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(RetailerID, Period) %>%
        dplyr::mutate(ret_meters = sum(TotalMeters)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(whole_meters > 100, ret_meters > 100) %>%
        dplyr::left_join(tp_details, by = c("RetailerID" = "Trading.Party.ID")) %>%
        dplyr::rename(Retailer = TradingPartyName, ShortRet = ShortName) %>%
        dplyr::left_join(tp_details, by = c("WholesalerID" = "Trading.Party.ID")) %>%
        dplyr::rename(Wholesaler = TradingPartyName, ShortWhole = ShortName) %>%
        dplyr::mutate(Pair = paste(ShortRet, "+", ShortWhole))

    }


    # Prep longunread data ----------------------------------------------------


    last12_long <- longunread %>%
      dplyr::filter(Period == max(longunread$Period) %m-% months(12)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(
        april_percent = sum(Meters_Unread_in_12mo) / sum(TotalMeters),
        Total_Meters_april = sum(TotalMeters)
      ) %>%
      dplyr::filter(Total_Meters_april > 100) %>%
      dplyr::select(by, Total_Meters_april, april_percent)


    last1_long <- longunread %>%
      dplyr::filter(Period == max(longunread$Period) %m-% months(1)) %>%
      dplyr::group_by_at(.vars = by) %>%
      dplyr::summarise(last_percent = sum(Meters_Unread_in_12mo) / sum(TotalMeters)) %>%
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
      dplyr::left_join(., last12_long, by = by) %>%
      dplyr::mutate(
        change = percent - april_percent,
        "Change in size" = dplyr::case_when(
          (TotalMeters - Total_Meters_april) / Total_Meters_april > 0.2 ~
            "Meter numbers increased by 20%",
          (TotalMeters - Total_Meters_april) / Total_Meters_april < 0.2 &
            (TotalMeters - Total_Meters_april) / Total_Meters_april > -0.2 ~
            "Meter number within +/- 20%",
          (TotalMeters - Total_Meters_april) / Total_Meters_april < -0.2 ~
            "Meter numbers reduced by > 20%",
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
