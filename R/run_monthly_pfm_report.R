
#' Run Monthly PFM Report
#'
#' @param tp.list character
#' @param excluded.list character
#' @param prep.data character
#' @param load.data character
#' @param output.dir character
#' @param excluded.list character
#' @param rmd.file  character
#' @param output.file character
#' @param my.dir character
#' @param data.period date
#'
#' @return
#' @export
#' @importFrom lubridate %m-%
#'
#' @examples

run_monthly_pfm_report <- function(
  my.dir = getwd(),
  tp.list = NULL,
  prep.data = TRUE,
  load.data = TRUE,
  excluded.list = NULL,
  rmd.file = "MonthlyPfmReport_pdf.Rmd",
  output.file =
    paste0(
      TRADING.PARTY,
      "_pfm-report_",
      format(Sys.Date(), "%Y-%m"),
      ".pdf"
    ),
  output.dir =
    paste0(
      my.dir,
      "/PfmReports/",
      format(Sys.Date(), "%Y-%m")
      ),
  data.period = Sys.Date() %m-% months(1)
  ) {

  if (prep.data) {

    my_dir <- my.dir

    MOSLR::process_monthly_tracker_mps(my.dir = my_dir)
    #MOSLR::process_monthly_tracker_ops()

  }

  if (load.data) {

    tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
    MPS_details <- utils::read.csv(paste0(my.dir, "/data/inputs/MPS_details.csv")) %>%
      dplyr::mutate(MPS = as.character(MPS))
    mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
    mps_data_melt <- readRDS(paste0(my.dir, "/data/rdata/mps_data_melt.Rda"))
    perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))
    mps_summary <- readRDS(paste0(my.dir, "/data/rdata/mps_summary.Rda"))

  }

  if (is.null(excluded.list)) {

    excluded_list = c("AQUAFLOW-R")

  } else {

    excluded_list <- excluded.list

  }

  if (is.null(tp.list)) {

    render_list <- mps_data_clean %>%
      dplyr::filter(!Trading.Party.ID %in% excluded.list) %>%
      dplyr::select(Trading.Party.ID) %>%
      dplyr::droplevels() %>%
      dplyr::mutate(
        Trading.Party.ID = as.character(Trading.Party.ID)
        )
    render_list <- unique(render_list$Trading.Party.ID)

  } else {

    render_list <- tp.list

  }

  for (TRADING.PARTY in render_list) {

    TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]
    SHORT.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]

    rmarkdown::render(
      input = system.file("rmd", rmd.file, package = "MOSLR"),
      output_file = output.file,
      output_dir = output.dir
    )

  }
  }

