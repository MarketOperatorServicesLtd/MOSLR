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
  conf.loc = NULL,
  conf.from = "datamgmt3",
  conf.to = "sandpit",
  query = NULL,
  saveDB = TRUE,
  output.table = "PERF_OPSData",
  overwrite = TRUE,
  append = FALSE,
  save.csv = FALSE,
  csv.file = "/data/inputs/OPS_data.csv"
) {

  if(is.null(query)){
    query <- readr::read_file(ifelse(
      file.exists(paste0(my.dir, "/data/inputs/OPSquery.txt")),
      paste0(my.dir, "/data/inputs/OPSquery.txt"),
      choose.files(caption = "Select file with the OPS query")))
  }


  Sys.setenv(R_CONFIG_ACTIVE = conf.from)

  if(is.null(conf.loc)){
    err <-  try(conf <- config::get(), TRUE)
    if("try-error" %in% class(err)) conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else if( conf.loc == "select"){
    conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else{
    conf <- config::get(file = conf.loc)
  }


  con.from <- odbc::dbConnect(odbc::odbc(),
                         Driver = conf$Driver,
                         Server = conf$Server,
                         Port = conf$Port,
                         trusted_connection = conf$trusted_connection)

  ops.data <- odbc::dbGetQuery(con.from, query)

  if(save.csv) write.csv(ops.data, paste0(my.dir, csv.file), row.names = FALSE)
  odbc::dbDisconnect(con.from)

  if(saveDB){

    Sys.setenv(R_CONFIG_ACTIVE = conf.to)
    conf <- config::get(file = ifelse(
      file.exists(paste0(my.dir, "/data/inputs/config.yml")),
      paste0(my.dir, "/data/inputs/config.yml"),
      choose.files(caption = "Select configuration file")))

    con.to <- odbc::dbConnect(odbc::odbc(),
                           Driver = conf$Driver,
                           Server = conf$Server,
                           Database = conf$Database,
                           Port = conf$Port,
                           trusted_connection = conf$trusted_connection)


    odbc::dbWriteTable(con.to, output.table, ops.data, overwrite = overwrite, append = append)

    odbc::dbDisconnect(con.to)
  }

  invisible(ops.data)


}
