
#' Render MPOP Report
#'
#' This functions prepares and loads the required
#' datasets and renders the monthly MPOP report
#'
#' @param rmd.file character
#' @param my.dir character
#' @param load.data boolean
#' @param dir.output character
#' @param file.name character
#' @param data.period date
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'


render_MPOP_report <- function(
  rmd.file = "MPOP.Rmd",
  my.dir = getwd(),
  load.data = TRUE,
  dir.output = paste0(my.dir, "/LUMsVacancyReport"),
  file.name = paste0("LUMsVacancyReport-", as.character(format(Sys.Date(), "%Y-%m")), ".pdf"),
  data.period = Sys.Date() %m-% months(1),
  ...
  )
  {

  # Setup -------------------------------------------------------------------

  my_dir <- my.dir
  data.period <- as.Date(data.period)
  lubridate::day(data.period) <- 1

  if (load.data) {
    tp_details <-
      readr::read_csv(paste0(my_dir, "/data/inputs/tp_details.csv")) %>%
      dplyr::mutate(
        TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
        ShortName = stringr::str_replace_all(ShortName, "&", "And")
      )

    longunread <-
      readr::read_csv(paste0(my_dir, "/data/inputs/longunread.csv")) %>%
      #dplyr::mutate(Period = as.Date(paste0(Period, "-01"), "%Y-%m-%d")) %>%
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


    vacant <-
      readr::read_csv(paste0(my_dir, "/data/inputs/vacancy.csv")) %>%
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
      #dplyr::mutate(Period = as.Date(Period, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))) %>%
      dplyr::left_join(tp_details, by = c("RetailerID" = "Trading.Party.ID")) %>%
      dplyr::rename(
        Retailer = TradingPartyName,
        ShortRet = ShortName
        ) %>%
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

    vacant_TP <- dplyr::bind_rows(
      readr::read_csv(paste0(my_dir, "/data/inputs/vacancy.csv")) %>%
        dplyr::group_by(RetailerID, Period) %>%
        dplyr::summarise(
          Premises = sum(Premises, na.rm = TRUE),
          Vacant_Premises = sum(VacantPremises, na.rm = TRUE)
          ) %>%
        dplyr::rename(TradingPartyID = RetailerID),
      readr::read_csv(paste0(my_dir, "/data/inputs/vacancy.csv")) %>%
        dplyr::group_by(WholesalerID, Period) %>%
        dplyr::summarise(
          Premises = sum(Premises),
          Vacant_Premises = sum(VacantPremises)
        ) %>%
        dplyr::rename(TradingPartyID = WholesalerID)
      ) %>%
      dplyr::mutate(
        percent = paste(round(100 * (Vacant_Premises) / (Premises), 1), "%"),
        Period = as.Date(Period, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))) %>%
      dplyr::left_join(tp_details, by = c("TradingPartyID" = "Trading.Party.ID"))

    readr::write_csv(vacant_TP, file = paste0(my_dir, "/data/inputs/Vacancy_TP.csv"))

    vacant_Total <-
      readr::read_csv(paste0(my_dir, "/data/inputs/vacancy.csv")) %>%
      dplyr::group_by(Period) %>%
      dplyr::summarise(
        Total_Premises = sum(Premises, na.rm = TRUE),
        Total_Vacant_Premises = sum(VacantPremises, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        #Period = as.Date(Period, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
        Market_Vacancy_Rate = Total_Vacant_Premises / Total_Premises
      )

    readr::write_csv(vacant_Total, file = paste0(my_dir, "/data/inputs/Vacancy_Total.csv"))

  }


  # Render MPOP report ------------------------------------------------------

  if (!dir.exists(dir.output)) {
    dir.create(dir.output)
  }

  rmarkdown::render(
    system.file("rmd", rmd.file, package = "MOSLR"),
    output_file = paste0(dir.output, "/", file.name)
  )


}
