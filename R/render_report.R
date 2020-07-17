
#' Render Report
#'
#' A generic function to prepare and load the required
#' datasets and run either PFM, Performance
#' or MPOP report
#'
#' @param report.type
#' @param my.dir
#' @param prep.data
#' @param load.data
#' @param data.period
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'

render_report <- function(
  report.type = NULL,
  my.dir = getwd(),
  prep.data = TRUE,
  load.data = TRUE,
  data.period = Sys.Date() %m-% months(1),
  ...
) {
    if(!all(report.type %in% c("Pfm", "Perf", "MPOP"))){
    stop("Variable 'report.type' in render_report has to be one or combination of c('Pfm', 'Perf', 'MPOP')")
  }

  if("Pfm" %in% report.type){
    MOSLR::render_PFM_report(
      my.dir = my.dir,
      data.period = data.period,
      prep.data = prep.data,
      load.data = load.data)
  }

  if("Perf" %in% report.type){
    MOSLR::render_PERF_report(
      my.dir = my.dir,
      data.period = data.period,
      prep.data = prep.data,
      load.data = load.data
      )
  }

    if("MPOP" %in% report.type){
      MOSLR::render_MPOP_report(
        my.dir = my.dir,
        load.data = load.data,
        data.period = data.period
      )
    }
}








