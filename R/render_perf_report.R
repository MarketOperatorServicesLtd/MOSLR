
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


# Render Monthly Performance Report ---------------------------------------

  rmarkdown::render(
    system.file("rmd", rmd.file, package = "MOSLR"),
    output_file = paste0(dir.output, "/", file.name)
    )

}
