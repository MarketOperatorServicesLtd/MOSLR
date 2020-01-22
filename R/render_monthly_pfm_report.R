
#' Title
#'
#' @param dir
#' @param rmd.file
#' @param TRADING.PARTY
#' @param output.file
#' @param output.dir
#'
#' @return
#' @export
#'
#' @examples

render_monthly_pfm_report <- function(
  dir,
  rmd.file = "MonthlyPfmReport_word.Rmd",
  TRADING.PARTY,
  output.file =
    paste0(
      TRADING.PARTY,
      "_pfm-report_",
      format(Sys.Date(), "%Y-%m"),
      ".pdf"
      ),
  output.dir =
    paste0(
      dir,
      "/PfmReports/",
      format(Sys.Date(), "%Y-%m")
    )
  ) {

  TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]
  SHORT.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]

  rmarkdown::render(
    input = system.file("rmd", rmd.file, package = "MOSLR"),
    output_file = output.file,
    output_dir = output.dir
    )

}
