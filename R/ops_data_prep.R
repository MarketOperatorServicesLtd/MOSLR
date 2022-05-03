
#' OPS Data Preparation
#'
#' Prepares the raw OPS data  so it is ready to be processed.
#'
#' @param ops.data character
#' @param csv.outputs character
#' @param rda.outputs character
#' @param my.dir character
#' @param save.output boolean
#' @param ops.details character
#' @param ops.thresholds character
#' @param DataBase boolean
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

ops_data_prep <- function(
  my.dir = getwd(),
  conf.loc = NULL,
  csv.outputs = paste0(my.dir, "/data/outputs"),
  rda.outputs = paste0(my.dir, "/data/rdata"),
  save.output = TRUE,
  DataBase = TRUE
  ) {




  if(DataBase) {

    Sys.setenv(R_CONFIG_ACTIVE = "sandpit")

    if(is.null(conf.loc)){
      err <-  try(conf <- config::get(), TRUE)
      if("try-error" %in% class(err)) conf <- config::get(file = choose.files(caption = "Select configuration file"))
    } else if( conf.loc == "select"){
      conf <- config::get(file = choose.files(caption = "Select configuration file"))
    } else{
      conf <- config::get(file = conf.loc)
    }


    # con <- odbc::dbConnect(odbc::odbc(),
    #                        Driver = conf$Driver,
    #                        Server = conf$Server,
    #                        Database = conf$Database,
    #                        Port = conf$Port,
    #                        trusted_connection = conf$trusted_connection)

    ops.data <- MOSLR::data_copy(my.dir = my.dir, conf.loc = conf.loc)


  } else {
      ops.data <- utils::read.csv(paste0(my.dir, "/data/inputs/OPS_data.csv"))
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

  ops.thresholds <- AzureStor::storage_read_csv(cont, "PerfReports/data/inputs/Standards_thresholds.csv") %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y")
    )

  ops.details <- AzureStor::storage_read_csv(cont, "PerfReports/data/inputs/Standards_details.csv") #%>%
    # dplyr::mutate(
    #   Details = iconv(Details),
    #   Context = iconv(Context)
    # )



  # Cleaning data -----------------------------

  ops_data_clean <- ops.data %>%
    dplyr::rename(
      TaskVolumeCompleted = TasksCompletedWithinPeriod,
      TaskVolumeOutstanding = TasksOutstandingEndPeriod,
      OnTimeTasksCompleted = TasksCompletedWithinTime,
      OnTimeTasksOutstanding = TasksOutstandingWithinTime
      ) %>%
    dplyr::mutate(
      Period = as.Date(Period),
      TaskCompletion = OnTimeTasksCompleted / TaskVolumeCompleted,
      OutstandingOntime = OnTimeTasksOutstanding / TaskVolumeOutstanding
      ) %>%
    dplyr::mutate_at(dplyr::vars(TaskCompletion, OutstandingOntime), ~replace(., is.nan(.), NA)) %>%
    dplyr::filter(!Standard %in% c(
      "OPS G4a",
      "OPS G4b",
      "OPS A1a",
      "OPS A2a",
      "OPS A2b",
      "OPS A2c",
      "OPS A3a",
      "OPS A3b",
      "OPS A4a"
      )
      )

  tasks_completed <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID,
      Period,
      Standard,
      TaskCompletion,
      TaskVolumeCompleted,
      OnTimeTasksCompleted
      ) %>%
    dplyr::rename(
      Performance = TaskCompletion,
      TaskVolume = TaskVolumeCompleted,
      OnTimeTasks = OnTimeTasksCompleted
      ) %>%
    dplyr::mutate(PerformanceMeasure = "Completed",
      Charges = (TaskVolume - OnTimeTasks) * 40
      )

  tasks_outstanding <- ops_data_clean %>%
    dplyr::select(
      Trading.Party.ID,
      Period,
      Standard,
      OutstandingOntime,
      TaskVolumeOutstanding,
      OnTimeTasksOutstanding
      ) %>%
    dplyr::rename(
      Performance = OutstandingOntime,
      TaskVolume = TaskVolumeOutstanding,
      OnTimeTasks = OnTimeTasksOutstanding
      ) %>%
    dplyr::mutate(
      PerformanceMeasure = "Outstanding",
      Charges = 0
      )

  ops_data_clean <- base::rbind(tasks_completed, tasks_outstanding)

  ops_thresholds <- ops.thresholds %>%
    dplyr::mutate(Period = as.Date(Period, format = "%d/%m/%Y"))


# Creating summary --------------------------------------------------------

  ops_summary <- ops_data_clean %>%
    dplyr::filter(!is.na(Performance)) %>%
    dplyr::group_by(Period, Standard, PerformanceMeasure) %>%
    dplyr::summarise(
      MarketMean = mean(Performance, na.rm = TRUE),
      MarketMedian = median(Performance, na.rm = TRUE),
      MarketTaskVolume = sum(TaskVolume, na.rm = TRUE)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Standard, Period)

  ops_data_clean <- ops_data_clean %>%
    dplyr::left_join(ops_summary, by = c("Period", "Standard", "PerformanceMeasure")) %>%
    dplyr::left_join(ops_thresholds, by = c("Standard", "Period", "PerformanceMeasure")) %>%
    dplyr::group_by(Trading.Party.ID, Standard, PerformanceMeasure) %>%
    dplyr::mutate(Threshold = zoo::na.locf(Threshold, na.rm = FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ops.details, by = c("Standard")) %>%
    dplyr::mutate(
      TaskShare = TaskVolume / MarketTaskVolume,
      BelowPeer = dplyr::if_else (
        !is.na(Threshold) > 0,
        Performance < Threshold,
        Performance < MarketMean
        ),
      Standard = factor(
        Standard,
        levels = c(
          "OPS A1a", "OPS A2a", "OPS A2b", "OPS A2c", "OPS A3a",
          "OPS A3b", "OPS A4a",
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b")
        )
      )


  if (save.output) {

    if(DataBase){
      # odbc::dbWriteTable(con, "PERF_OPSSummary", ops_summary, overwrite = TRUE)
      # odbc::dbWriteTable(con, "PERF_OPSDataClean", ops_data_clean, overwrite = TRUE)

      AzureStor::storage_write_csv(ops_summary, cont, "PerfReports/data/inputs/ops_summary.csv")
      AzureStor::storage_write_csv(ops_data_clean, cont, "PerfReports/data/inputs/OPS_data_clean.csv")
    } else{
       saveRDS(ops_summary, file = paste0(rda.outputs, "/ops_summary.Rda"))
    utils::write.csv(ops_data_clean, paste0(csv.outputs, "/ops_data_clean.csv"))
    saveRDS(ops_data_clean, file = paste0(rda.outputs, "/ops_data_clean.Rda"))
    }

  }

  #if(DataBase) odbc::dbDisconnect(con)
  invisible(ops_data_clean)

}
