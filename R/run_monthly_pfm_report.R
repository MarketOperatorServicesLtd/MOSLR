
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
#'
#' @return
#' @export
#'
#' @examples

run_monthly_pfm_report <- function(
  my.dir = getwd(),
  tp.list = NULL,
  prep.data = TRUE,
  load.data = TRUE,
  excluded.list = NULL,
  rmd.file = "MonthlyPfmReport_word.Rmd",
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
      )
  ) {

  if (prep.data) {

    prep_mps_render_monthly_pfm_report()
    prep_ops_render_monthly_pfm_report()

  }

  if (load.data) {

    tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
    mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
    mps_data_melt <- readRDS(paste0(my.dir, "/data/rdata/mps_data_melt .Rda"))
    charges_graph_mps <- readRDS(paste0(my.dir, "/data/rdata/charges_graph_mps .Rda"))
    charges_table_mps <- readRDS(paste0(my.dir, "/data/rdata/charges_table_mps.Rda"))
    tracking_mps <- readRDS(paste0(my.dir, "/data/rdata/tracking_mps.Rda"))
    mps_summary <- readRDS(paste0(my.dir, "/data/rdata/mps_summary.Rda"))

  }

  if (is.null(excluded.list)) {

    excluded_list = c("AQUAFLOW-R")

  } else {

    excluded_list <- excluded.list

  }

  if (is.null(tp.list)) {

    render_list <- mps_data %>%
      filter(!Trading.Party.ID %in% excluded.list) %>%
      select(Trading.Party.ID) %>%
      droplevels() %>%
      mutate(Trading.Party.ID = as.character(Trading.Party.ID))
    render_list <- unique(render_list$Trading.Party.ID)

  } else {

    render_list <- tp.list

  }

  for (TRADING.PARTY in render_list) {

    TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]
    SHORT.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]

    mps_data_clean
    mps_data_melt
    charges_graph_mps
    charges_table_mps
    tracking_mps

    rmarkdown::render(
      input = system.file("rmd", rmd.file, package = "MOSLR"),
      output_file = output.file,
      output_dir = output.dir
    )

  }
  }

