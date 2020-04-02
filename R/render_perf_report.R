
#' Render Performance Report
#'
#' This function renders the Monthly Performance Report
#' to be shared with MPC.
#'
#' @param dir.output character
#' @param file.name character
#' @param ... foo
#' @param rmd.file character
#' @param my.dir character
#' @param prep.data logical
#' @param load.data logical
#' @param data.period date
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom kableExtra kable_styling
#' @importFrom magrittr %>%
#'
#' @examples

render_PERF_report <- function(
  rmd.file = "perf_report_main.Rmd",
  my.dir = getwd(),
  prep.data = TRUE,
  load.data = TRUE,
  dir.output = paste0(my.dir, "/MonthlyPerfReport"),
  file.name =
    paste0("MonthlyPerformanceReport-", as.character(format(Sys.Date(), "%Y-%m")), ".pdf"),
  data.period = Sys.Date() %m-% months(1),
  ...
  ) {



# Setup -------------------------------------------------------------------

  my_dir <- my.dir
  data.period <- as.Date(data.period)
  lubridate::day(data.period) <- 1

  if (prep.data) {

    MOSLR::mps_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period)
    MOSLR::ops_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period)

  }

  if (load.data) {

    tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))%>%
      dplyr::mutate(
        TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
        ShortName = stringr::str_replace_all(ShortName, "&", "And"))

    Standards_details <- utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv")) %>%
      dplyr::mutate(Standard = as.character(Standard))

    mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
    perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))
    mps_summary <- readRDS(paste0(my.dir, "/data/rdata/mps_summary.Rda"))

    ops_data_clean <- readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda"))
    perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))
    ops_summary <- readRDS(paste0(my.dir, "/data/rdata/ops_summary.Rda"))

    mps_aggregate_perf <- MOSLR::mps_aggregate_perf(df = mps_data_clean, tp.details = tp_details)
    ops_aggregate_perf <- MOSLR::ops_aggregate_perf(df = ops_data_clean, tp.details = tp_details)

    longunread <- utils::read.csv(paste0(my.dir, "/data/inputs/longunread.csv"))%>%
      dplyr::mutate(
        Period =
          as.Date(paste0(Period, "-01"), "%Y-%m-%d")
      )%>%
      dplyr::group_by(
        WholesalerID, Period
      )%>%
      dplyr::mutate(
        whole_meters = sum(Total_Meters)
      )%>%
      dplyr::ungroup()%>%
      dplyr::group_by(
        RetailerID, Period
      )%>%
      dplyr::mutate(
        ret_meters = sum(Total_Meters)
      )%>%
      dplyr::ungroup()%>%
      dplyr::filter(
        whole_meters > 100,
        ret_meters > 100)%>%
      dplyr::left_join(
        .,
        tp_details,
        by = c("RetailerID" = "Trading.Party.ID"))%>%
      dplyr::rename(
        Retailer = TradingPartyName,
        ShortRet = ShortName
      )%>%
      dplyr::left_join(
        .,
        tp_details,
        by = c("WholesalerID" = "Trading.Party.ID"))%>%
      dplyr::rename(
        Wholesaler = TradingPartyName,
        ShortWhole = ShortName
      )%>%
      dplyr::mutate(Pair = paste(ShortRet, "+", ShortWhole))


    vacant <- utils::read.csv(paste0(my.dir, "/data/inputs/Vacancy_Pairing.csv"))%>%
      dplyr::group_by(
        WholesalerID, Period
      )%>%
      dplyr::mutate(
        whole_vacant = sum(Premises)
      )%>%
      dplyr::ungroup()%>%
      dplyr::group_by(
        RetailerID, Period)%>%
      dplyr::mutate(
        ret_vacant = sum(Premises)
      )%>%
      dplyr::ungroup()%>%
      dplyr::filter(
        whole_vacant > 200,
        ret_vacant > 200
      )%>%
      dplyr::mutate(
        Period = as.Date(Period)
      )%>%
      dplyr::left_join(
        .,
        tp_details,
        by = c("RetailerID" = "Trading.Party.ID"))%>%
      dplyr::rename(
        Retailer = TradingPartyName,
        ShortRet = ShortName
      )%>%
      dplyr::left_join(
        .,
        tp_details,
        by = c("WholesalerID" = "Trading.Party.ID"))%>%
      dplyr::rename(
        Wholesaler = TradingPartyName,
        ShortWhole = ShortName
      )%>%
      dplyr::mutate(
        Pair = stringr::str_replace_all(paste(ShortRet, "+", ShortWhole), "&", "And"),
        Retailer = stringr::str_replace_all(Retailer, "&", "And"),
        Wholesaler = stringr::str_replace_all(Wholesaler, "&", "And"))

    vacant_TP <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_TP.csv"))%>%
      dplyr::mutate(percent = paste(round(100*(Vacant_Premises)/(Premises),1),"%"))%>%
      dplyr::left_join(
        ., tp_details,
        by = c("TradingPartyID" = "Trading.Party.ID")
      )

    vacant_Total <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_Total.csv"))


  }


# Render Monthly Performance Report ---------------------------------------

  rmarkdown::render(
    system.file("rmd", rmd.file, package = "MOSLR"),
    output_file = paste0(dir.output, "/", file.name)
    )

}
