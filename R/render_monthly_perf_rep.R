
#' Render Rmarkdown Monthly Performance Report
#'
#' This function renders the Monthly Performance Report
#' using the relevant .Rmd file.
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

render_monthly_perf_rep <- function(
  dir = getwd(),
  dir.output = paste0(dir, "/MonthlyPerfReport"),
  file.name =
    paste0("MonthlyPerformanceReport-", as.character(format(Sys.Date(), "%Y-%m")), ".pdf"),
  ...
  ) {

# Render Monthly Performance Report ---------------------------------------

  rmarkdown::render(
    system.file("rmd", "MonthlyPerformanceReport.Rmd", package = "MOSLR"),
    output_file = paste0(dir.output, "/", file.name)
    )

}
