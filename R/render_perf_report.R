
#' Render Performance Report
#'
#' This function renders the Monthly Performance Report
#' to be shared with MPC.
#'
#' @param dir character
#' @param dir.output character
#' @param file.name character
#' @param ... foo
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom kableExtra kable_styling
#'
#' @examples

render_PERF_report <- function(
  rmd.file = "MonthlyPerformanceReport.Rmd",
  my.dir = getwd(),
  dir.output = paste0(my.dir, "/MonthlyPerfReport"),
  file.name =
    paste0("MonthlyPerformanceReport-", as.character(format(Sys.Date(), "%Y-%m")), ".pdf"),
  ...
  ) {

# Render Monthly Performance Report ---------------------------------------

  rmarkdown::render(
    system.file("rmd", rmd.file, package = "MOSLR"),
    output_file = paste0(dir.output, "/", file.name)
    )

}
