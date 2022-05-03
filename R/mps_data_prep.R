
#' MPS Data Preparation
#'
#'Prepares the raw MPS data so it is ready to be processed.
#'
#' @param my.dir character
#' @param mps.data character
#' @param mps.thresholds character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param Standards.details character
#' @param save.output logical
#'
#' @return
#' @export
#'
#' @importFrom stats median
#' @importFrom magrittr %>%
#'
#' @examples

mps_data_prep <- function(
  my.dir = getwd(),
  conf.loc = NULL,
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  save.output = TRUE,
  DataBase = TRUE
  ) {





  # Importing raw data ------------------------------------------------------
  if(DataBase){
    Sys.setenv(R_CONFIG_ACTIVE = "sandpit")

    if(is.null(conf.loc)){
    err <-  try(conf <- config::get(), TRUE)
    if("try-error" %in% class(err)) conf <- config::get(file = choose.files(caption = "Select configuration file"))
    } else if( conf.loc == "select"){
      conf <- config::get(file = choose.files(caption = "Select configuration file"))
    } else{
      conf <- config::get(file = conf.loc)
    }


    con <- odbc::dbConnect(odbc::odbc(),
                   Driver = conf$Driver,
                   Server = conf$Server,
                   Database = conf$Database,
                   Port = conf$Port,
                   trusted_connection = conf$trusted_connection)

    mps_data <- dplyr::tbl(con, "PERF_MPSData") %>% dplyr::as_tibble()


  } else{
    mps_data <- utils::read.csv(paste0(my.dir, "/data/inputs/MPS_data.csv"))



  }


  Sys.setenv(R_CONFIG_ACTIVE = "digitaldata")

  if(is.null(conf.loc)){
    err <-  try(conf <- config::get(), TRUE)
    if("try-error" %in% class(err)) conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else if( conf.loc == "select"){
    conf <- config::get(file = choose.files(caption = "Select configuration file"))
  } else{
    conf <- config::get(file = conf.loc)
  }

  bl_endp_key <- AzureStor::storage_endpoint(endpoint = conf$endpoint, sas = conf$sas)
  cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")

  mps_thresholds <- AzureStor::storage_read_csv(cont, "PerfReports/data/inputs/Standards_thresholds.csv") %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y")
    )

  Standards.details <- AzureStor::storage_read_csv(cont, "PerfReports/data/inputs/Standards_details.csv")


  # Importing and cleaning MPS data -----------------------------------------

  mps_data_clean <- mps_data %>%
    dplyr::rename(
      Charges = Total.Performance.Charge.Value,
      TaskVolume = Total.number.of.tasks.compeleted.within.Period,
      OnTimeTasks = Number.of.tasks.completed.on.time
      ) %>%
    dplyr::mutate(
      Period = zoo::as.Date(zoo::as.yearmon(Period)),
      Performance = OnTimeTasks / TaskVolume,
      PerformanceMeasure = "Completed"
      ) %>%
    dplyr::mutate_at(dplyr::vars(Performance), ~replace(., is.nan(.), NA)) %>%
    dplyr::arrange(
      Period, Trading.Party.ID, Standard
      ) %>%
    dplyr::select(
      Period,
      Trading.Party.ID,
      Standard,
      PerformanceMeasure,
      Performance,
      TaskVolume,
      Charges,
      OnTimeTasks
      ) %>%
    dplyr::left_join(
      Standards.details,
      by = c("Standard")
      ) %>%
    dplyr::left_join(
      mps_thresholds,
      by = c("Standard", "Period", "PerformanceMeasure")
      ) %>%
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
    dplyr::mutate(
      Threshold = zoo::na.locf(Threshold, na.rm = FALSE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Standard = factor(
        Standard,
        levels = c(
          "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
          "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
          "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19"
        )),
      key = as.factor(paste(Trading.Party.ID, Standard))
      )


  # Creating summary grouped by Standard with market metrics by month ------------

  mps_summary <- mps_data_clean %>%
    dplyr::filter(!is.na(Performance)) %>%
    dplyr::group_by(Period, Standard) %>%
    dplyr::summarise(
      MarketMean = mean(Performance, na.rm = TRUE),
      MarketMedian = median(Performance, na.rm = TRUE),
      MarketTaskVolume = sum(TaskVolume)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Standard, Period)

  mps_data_clean <-
    dplyr::left_join(
      mps_summary,
      mps_data_clean,
      by = c("Period", "Standard")
      ) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / MarketTaskVolume,
      BelowPeer = dplyr::if_else(
        !is.na(Threshold),
        Performance < Threshold,
        Performance < MarketMean
        )
      )

  if (save.output) {

    if(DataBase){
      # odbc::dbWriteTable(con, "PERF_MPSSummary", mps_summary, overwrite = TRUE)
      # odbc::dbWriteTable(con, "PERF_MPSDataClean", mps_data_clean, overwrite = TRUE)
      #
      AzureStor::storage_write_csv(mps_summary, cont, "PerfReports/data/inputs/mps_summary.csv")
      AzureStor::storage_write_csv(mps_data_clean, cont, "PerfReports/data/inputs/MPS_data_clean.csv")

    } else{
        saveRDS(mps_summary, file = paste0(rda.outputs, "/mps_summary.Rda"))
    utils::write.csv(mps_data_clean, paste0(csv.outputs, "/MPS_data_clean.csv"))
    saveRDS(mps_data_clean, file = paste0(rda.outputs, "/mps_data_clean.Rda"))
      }
  }

  if(DataBase) odbc::dbDisconnect(con)

  invisible(mps_data_clean)

}
