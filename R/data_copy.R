#' Copy data from one database to another
#'
#' data_copy() is designed to copy data from one database to another.
#' By default, it copies the OPS data from data-mgmt3 to data-mgmt.
#' Can also save the table as .csv file
#'
#' @param my.dir working directory
#' @param server.from server name for the source data
#' @param server.to server name to where data is copied to
#' @param db.to database name to where data is copied to
#' @param query SQL query that gets the source data
#' @param saveDB should the data be saved to a new database
#' @param output.table name of the output table in a database
#' @param overwrite should the existing table be overwritten
#' @param append should the existing table be appended to
#' @param save.csv should a csv file be saved
#' @param csv.file name of the csv file
#'
#' @return
#' @export
#'
#' @examples
#'
data_copy <- function(
  my.dir = getwd(),
  server.from = "data-mgmt3",
  server.to = "data-mgmt",
  db.to = "MOSL_Sandpit",
  query = NULL,
  saveDB = TRUE,
  output.table = "PERF_OPSData",
  overwrite = TRUE,
  append = FALSE,
  save.csv = FALSE,
  csv.file = "/data/inputs/OPS_data.csv"
) {

  if(is.null(query)){
    query <- "SELECT  tp.TradingPartyName as [Trading.Party.ID]
      , sub.Period
	    , x.Standard
      , x.TasksStartedWithinPeriod
      , x.TasksCompletedWithinPeriod
      , x.TasksCompletedWithinTime
      , x.TasksCompletedPermittedDeferral
      , x.TasksCompletedExtremelyLate
      , x.TasksRejectedWithinPeriod
      , x.TasksOutstandingEndPeriod
      , x.TasksOutstandingWithinTime
      , x.TasksOutstandingOutsideTime
      , x.TasksOutstandingOutsideTimeExtremelyLate
FROM [MOSL_MarketPerformance].[dbo].[OPSPerformance] x

RIGHT JOIN [MOSL_MarketPerformance].[dbo].[vwOPSLatestValidSubmissions] sub
	ON x.SubmissionID = sub.SubmissionID

RIGHT JOIN [BatchTransactionServer].[dbo].[TradingParties] tp
	ON sub.TradingPartyID = tp.TradingPartyID

WHERE LEFT(x.Standard, 5) <> 'OPS A' AND
	(x.TasksStartedWithinPeriod + x.TasksCompletedWithinPeriod
	+ x.TasksRejectedWithinPeriod + x.TasksOutstandingEndPeriod) <> 0
	AND tp.TradingPartyName <> 'MOSLTEST-W'
	AND Period >= '2019-04-01'"
  }

  con.from <- odbc::dbConnect(odbc::odbc(),
                              Driver = "SQL Server",
                              Server = server.from,
                              Port = 1433,
                              trusted_connection="True")

  ops.data <- odbc::dbGetQuery(con.from, query)

  if(save.csv) write.csv(ops.data, paste0(my.dir, csv.file), row.names = FALSE)
  odbc::dbDisconnect(con.from)

  if(saveDB){

    con.to <- odbc::dbConnect(odbc::odbc(),
                              Driver = "SQL Server",
                              Server = server.to,
                              Database = db.to,
                              Port = 1433,
                              trusted_connection="True")

    odbc::dbWriteTable(con.to, output.table, ops.data, overwrite = overwrite, append = append)

    odbc::dbDisconnect(con.to)
  }

  invisible(ops.data)


}
