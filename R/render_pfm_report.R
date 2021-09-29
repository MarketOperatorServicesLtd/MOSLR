
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
#' @param my.dir character
#' @param data.period date
#'
#' @return
#' @export
#' @importFrom lubridate %m-%
#' @importFrom magrittr %>%
#'
#' @examples

render_PFM_report <- function(
  my.dir = getwd(),
  tp.list = NULL,
  prep.data = TRUE,
  load.data = TRUE,
  excluded.list = NULL,
  rmd.file = "pfm_report_main.Rmd",
  output.dir = paste0(my.dir, "/PfmReports/", format(Sys.Date(), "%Y-%m")),
  data.period = Sys.Date() %m-% months(1),
  include.aggregate = FALSE,
  include.consistency = FALSE,
  include.full.ranking = FALSE,
  DataBase = TRUE
  ) {

  my_dir <- my.dir
  data.period <- as.Date(data.period)

  lubridate::day(data.period) <- 1


  if (prep.data) {

    MOSLR::mps_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period, StandardKey = "MPS", DataBase = DataBase)
    MOSLR::mps_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period, StandardKey = "API", DataBase = DataBase)
    MOSLR::ops_process_tracker(my.dir = my_dir, save.output = TRUE, period.create = data.period, DataBase = DataBase)

  }

  if (load.data) {

    if(DataBase){
      con <- odbc::dbConnect(odbc::odbc(),
                             Driver = "SQL Server",
                             Server = "data-mgmt",
                             Database = "MOSL_Sandpit",
                             Port = 1433,
                             trusted_connection = "True")

      tp_details <- dplyr::tbl(con, "PERF_TPDetails") %>% dplyr::as_tibble()
      spid_counts <- dplyr::tbl(con, "PERF_SPIDCounts") %>% dplyr::as_tibble()
      Standards_details <- dplyr::tbl(con, "PERF_StandardsDetails") %>% dplyr::as_tibble()  %>%
        dplyr::mutate(Standard = as.character(Standard))

      api_data_clean <- dplyr::tbl(con, "PERF_APIDataClean") %>% dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period),
                      Threshold = as.numeric(Threshold))

      perf_status_api <- dplyr::tbl(con, "PERF_APIPERFStatus") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))

      api_summary <- api_data_clean %>%
        dplyr::group_by(Period, Standard) %>%
        dplyr::summarise(
          MarketMean = mean(MarketMean, na.rm = TRUE),
          MarketMedian = median(Performance, na.rm = TRUE),
          MarketTaskVolume = sum(TaskVolume)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Standard, Period)


      mps_data_clean <- dplyr::tbl(con, "PERF_MPSDataClean") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))
      perf_status_mps <- dplyr::tbl(con, "PERF_MPSPERFStatus") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))
      mps_summary <- dplyr::tbl(con, "PERF_MPSSummary") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))

      ops_data_clean <- dplyr::tbl(con, "PERF_OPSDataClean") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period),
                      Details = iconv(Details),
                      Context = iconv(Context))
      perf_status_ops <- dplyr::tbl(con, "PERF_OPSPERFStatus") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period),
                      Details = iconv(Details),
                      Context = iconv(Context))
      ops_summary <- dplyr::tbl(con, "PERF_OPSSummary") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))

      odbc::dbDisconnect(con)
    } else{
      tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))
    spid_counts <- utils::read.csv(paste0(my.dir, "/data/inputs/spid_counts.csv"))
    Standards_details <- utils::read.csv(paste0(my.dir, "/data/inputs/Standards_details.csv")) %>%
      dplyr::mutate(Standard = as.character(Standard))

    api_data_clean <- read.csv(paste0(my.dir, "/data/outputs/API_data_clean.csv"), fileEncoding="UTF-8-BOM") %>%
      dplyr::mutate(Period = as.Date(Period),
                    Threshold = as.numeric(Threshold))
    perf_status_api <- readRDS(paste0(my.dir, "/data/rdata/perf_status_api.Rda"))
    api_summary <- api_data_clean %>%
      dplyr::group_by(Period, Standard) %>%
      dplyr::summarise(
        MarketMean = mean(MarketMean, na.rm = TRUE),
        MarketMedian = median(Performance, na.rm = TRUE),
        MarketTaskVolume = sum(TaskVolume)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Standard, Period)


    mps_data_clean <- readRDS(paste0(my.dir, "/data/rdata/mps_data_clean.Rda"))
    perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))
    mps_summary <- readRDS(paste0(my.dir, "/data/rdata/mps_summary.Rda"))

    ops_data_clean <- readRDS(paste0(my.dir, "/data/rdata/ops_data_clean.Rda"))
    perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))
    ops_summary <- readRDS(paste0(my.dir, "/data/rdata/ops_summary.Rda"))
    }



    api_aggregate_perf <- MOSLR::mps_aggregate_perf(df = api_data_clean, tp.details = tp_details, spid.counts = spid_counts)
    mps_aggregate_perf <- MOSLR::mps_aggregate_perf(df = mps_data_clean, tp.details = tp_details, spid.counts = spid_counts)
    ops_aggregate_perf <- MOSLR::ops_aggregate_perf(df = ops_data_clean, tp.details = tp_details, spid.counts = spid_counts)

  }

  if (is.null(excluded.list)) {

    excluded_list = c(
      "AQUAFLOW-R", "ANGLIAN-R", "CHOLDERTON-R", "NORTHUM-R", "WATERSCAN-R"
      )

  } else {

    excluded_list <- excluded.list

  }

  if (is.null(tp.list)) {

    render_list <- mps_data_clean %>%
      dplyr::filter(
        !Trading.Party.ID %in% excluded_list
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


    endpoint_url <- "https://stmosldataanalyticswe.blob.core.windows.net/"
    sas <- readr::read_file(ifelse(file.exists(paste0(my.dir, "/data/inputs/digitaldata_sas.txt")), paste0(my.dir, "/data/inputs/digitaldata_sas.txt"), choose.files()))
    bl_endp_key <- AzureStor::storage_endpoint(endpoint = endpoint_url, sas = sas)
    cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")




  for (TRADING.PARTY in render_list) {

    # Trading Party names ----------------

    TRADING.PARTY.NAME <- tp_details$TradingPartyName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]
    SHORT.NAME <- tp_details$ShortName[tp_details$Trading.Party.ID == TRADING.PARTY, drop = TRUE]


    # API data ----------------

    api_data_clean_temp <- api_data_clean %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY
      ) %>%
      droplevels()

    perf_status_api_temp <- perf_status_api %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Period == data.period
      ) %>%
      droplevels()

    api_aggregate_perf_temp <- api_aggregate_perf %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Period == data.period
      ) %>%
      droplevels()


    # MPS data ----------------

    mps_data_clean_temp <- mps_data_clean %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY
        ) %>%
      droplevels()

    perf_status_mps_temp <- perf_status_mps %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Period == data.period
        ) %>%
      droplevels()

    mps_aggregate_perf_temp <- mps_aggregate_perf %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Period == data.period
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
        Period == data.period
        ) %>%
      droplevels()

    ops_aggregate_perf_temp <- ops_aggregate_perf %>%
      dplyr::filter(
        Trading.Party.ID == TRADING.PARTY,
        Period == data.period
        ) %>%
      droplevels()


    tryCatch(

      expr = {

        rmarkdown::render(
          input = system.file("rmd", rmd.file, package = "MOSLR"),
          output_file =
            paste0(TRADING.PARTY, "_performance-report_", format(Sys.Date(), "%Y-%m"), ".pdf"),
          output_dir = output.dir
          )

        AzureStor::storage_upload(cont,
                                  paste0(output.dir, "/", TRADING.PARTY, "_performance-report_", format(Sys.Date(), "%Y-%m"), ".pdf"),
                                  paste0("PerfReports/Reports/",TRADING.PARTY, "/", TRADING.PARTY, "_performance-report_", format(Sys.Date(), "%Y-%m"), ".pdf"))

      },

      error = function(e) {
        print(e)

      }
    )
  }
    if(DataBase) odbc::dbDisconnect(con)
  }

