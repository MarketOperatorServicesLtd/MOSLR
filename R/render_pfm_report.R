
#' Render PFM Report
#'
#' This functions prepares and loads the required
#' datasets and renders the monthly Portfolio Manager
#' reports that are sent to Trading Parties.
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

render_PFM_report <- function(
  my.dir = getwd(),
  tp.list = NULL,
  prep.data = TRUE,
  load.data = TRUE,
  excluded.list = NULL,
  rmd.file = "pfm_report_main.Rmd",
  output.dir =
    paste0(
      my.dir,
      "/PfmReports/",
      format(Sys.Date(), "%Y-%m")
      ),
  data.period = Sys.Date() %m-% months(1)
  ) {

  my_dir <- my.dir
  data.period <- as.Date(data.period)

  if (prep.data) {

    MOSLR::mps_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period)
    MOSLR::ops_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period)

  }

  if (load.data) {

    tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
    Standards_details <- utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv")) %>%
      dplyr::mutate(Standard = as.character(Standard))

    mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
    perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))
    mps_summary <- readRDS(paste0(my.dir, "/data/rdata/mps_summary.Rda"))

    ops_data_clean <- readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda"))
    perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))
    ops_summary <- readRDS(paste0(my.dir, "/data/rdata/ops_summary.Rda"))

    mps_aggregate_perf <- mps_aggregate_perf(df = mps_data_clean, tp.details = tp_details)
    ops_aggregate_perf <- ops_aggregate_perf(df = ops_data_clean, tp.details = tp_details)

  }

  if (is.null(excluded.list)) {

    excluded_list = c(
      "AQUAFLOW-R", "ANGLIAN-R", "AFFINITY-W", "ALBIONECO-R", "ALBION-R",
      "ALBION-w", "BLACKPOOL-R", "CHOLDERTON-R", "DEEVALLEY-W", "HEINEKEN-R",
      "ICOSA2-2", "INDWATER-W", "JLP-R", "KELDA-R", "MARSTONS-R", "NCC-R",
      "NORTHUM-R", "PEEL-W", "PEELWL-R", "SEVERNCON-W", "SOUTHSTAFF-R",
      "SOUTHSTAFF-W", "TWRC-R", "VEOLIA-R", "WATERSCAN-R", "WTRCHOICE-R"
      )

  } else {

    excluded_list <- excluded.list

  }

  if (is.null(tp.list)) {

    render_list <- mps_data_clean %>%
      dplyr::filter(
        !Trading.Party.ID %in% excluded.list
        ) %>%
      dplyr::select(Trading.Party.ID) %>%
      droplevels() %>%
      dplyr::mutate(
        Trading.Party.ID = as.character(Trading.Party.ID)
        )
    render_list <- unique(render_list$Trading.Party.ID)

  } else {

    render_list <- tp.list

  }

  for (TRADING.PARTY in render_list) {

    # Trading Party names ----------------

    TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]
    SHORT.NAME <- tp_details$ShortName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]

    # MPS data ----------------

    mps_data_clean_temp <- mps_data_clean %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY
        ) %>%
      droplevels()

    perf_status_mps_temp <- perf_status_mps %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Date == data.period
        ) %>%
      droplevels()

    mps_aggregate_perf_temp <- mps_aggregate_perf %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Date == data.period
        ) %>%
      droplevels()


    # OPS data ----------------

    ops_data_clean_temp <- ops_data_clean %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY
        ) %>%
      droplevels()

    perf_status_ops_temp <- perf_status_ops %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Date == data.period
        ) %>%
      droplevels()

    ops_aggregate_perf_temp <- ops_aggregate_perf %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Date == data.period
        ) %>%
      droplevels()


    tryCatch(

      expr = {

        rmarkdown::render(
          input = system.file("rmd", rmd.file, package = "MOSLR"),
          output_file =
            paste0(TRADING.PARTY, "_pfm-report_", format(Sys.Date(), "%Y-%m"), ".pdf"),
          output_dir = output.dir
          )

      },

      error = function(e) {
        print(e)

      }
    )
  }
  }

