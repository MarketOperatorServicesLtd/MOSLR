
#' Process Monthly MPS Tracker (Post Analysis and PfM Commentary)
#'
#' This function processes the MPS performance data for each
#' Trading Party by MPS after the analysis and PfM Commentary
#' has been added. This will then be used to  render the PfM
#' and Monthly Performance Reports created each month.
#'
#' @param save.output  logical
#' @param save.dir.rds character
#' @param save.dir.csv character
#' @param my.dir character
#' @param keep.vars logical
#' @param period.create date
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

mps_process_tracker <- function(
  my.dir = getwd(),
  save.output = TRUE,
  StandardKey = "MPS",
  save.dir.rds = paste0(my.dir, "/data/rdata/perf_status_", tolower(StandardKey), ".Rda"),
  save.dir.csv = paste0(my.dir, "/data/outputs/perf_status_", tolower(StandardKey), ".csv"),
  keep.vars = TRUE,
  period.create = Sys.Date() %m-% months(1),
  DataBase = TRUE
  ) {


  if(!StandardKey %in% c('MPS', 'API')){
    stop("Variable StandardKey has to be either 'MPS' or 'API'")
  }

# Setting parameters ------------------------------------------------------

  if (keep.vars) {
    var_list <- NULL
  } else {
    var_list <-
      c(
        "Period", "SecondaryCategory", "Trading.Party.ID", "Standard", "Action",
        "Rationale", "PFM_Commentary", "PerfFlag3m", "PerfFlag6m",
        "ActiveIPRP", "ActivePRP", "IPRPend", "PRPend", "MilestoneFlag", "Pending", "UnderReview",
        "OnWatchRectificationEnd",  "OnWatch", "Consistency", "PerfRating", "IPRPeligible",
        "CumWatch", "CumIPRP", "CumResubmit", "CumEscalate", "CumExtend",
        "PerformanceMeasure"
      )
  }

  if(DataBase){
    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "SQL Server",
                           Server = "data-mgmt",
                           Database = "MOSL_Sandpit",
                           Port = 1433,
                           trusted_connection = "True")
    }


# Importing data ----------------------------------------------------------

  my_dir <- my.dir

  period.create <- as.Date(period.create)

  monthly_tracking_pre <-
    MOSLR::mps_create_tracker(
      my.dir = my_dir,
      period = period.create,
      period.only = FALSE,
      save.output = FALSE,
      keep.vars = TRUE,
      filter.category = NULL,
      StandardKey = StandardKey,
      DataBase = DataBase
      ) %>%
    dplyr::select(
      -Action, - Rationale, -PFM_Commentary, -Template_Sent, -Response_Received_Template
      )

  endpoint_url <- "https://stmosldataanalyticswe.blob.core.windows.net/"
  sas <- readr::read_file(ifelse(file.exists(paste0(my.dir, "/data/inputs/digitaldata_sas.txt")), paste0(my.dir, "/data/inputs/digitaldata_sas.txt"), choose.files()))
  bl_endp_key <- AzureStor::storage_endpoint(endpoint = endpoint_url, sas = sas)
  cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")

  monthly_tracking_post <- AzureStor::storage_read_csv(cont, paste0("/PerfReports/tracking_", tolower(StandardKey), ".csv")) %>%
    dplyr::mutate(
      Period = as.Date(Period, format = "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
    ) %>%
    dplyr::select(
      Period, Trading.Party.ID, Standard, PerformanceMeasure, Action,
      Rationale, PFM_Commentary, Response_Received_Template
    )


# Full Joining IPRP status and mps tracking ----------------------------------

  perf_status <- monthly_tracking_pre %>%
    dplyr::full_join(
      monthly_tracking_post,
      by = c("Period", "Trading.Party.ID", "Standard", "PerformanceMeasure")
      ) %>%
    dplyr::mutate(
      Action = tolower(Action),
      Standard = factor(
        Standard,
        levels = c(
          "MPS 1", "MPS 2", "MPS 3", "MPS 4", "MPS 5", "MPS 6", "MPS 7",
          "MPS 8", "MPS 9", "MPS 10", "MPS 12", "MPS 13", "MPS 14",
          "MPS 15", "MPS 16", "MPS 17", "MPS 18", "MPS 19", "UPRN Completeness", "VOA Completeness", "GIS Issues"
          )
        )
      ) %>%
    dplyr::arrange(Period, Trading.Party.ID, Standard) %>%
    dplyr::group_by(Trading.Party.ID, Standard) %>%
    dplyr::mutate(
      Action = tidyr::replace_na(Action, ""),
      CumWatch = cumsum(OnWatch),
      CumIPRP = cumsum(Action == "iprp"),
      CumResubmit = cumsum(Action == "re-submit"),
      CumEscalate = cumsum(Action == "escalate"),
      CumExtend = cumsum(Action == "extend"),
      Action = stringr::str_to_sentence(Action)
      ) %>%
    dplyr::ungroup() %>%
    {if (!keep.vars) {
      dplyr::select(., var_list)
      } else {
        dplyr::select(., dplyr::everything())
        }
      }

  if(save.output) {

    if(DataBase){
      sql.field.types <- list(PFM_Commentary = "nvarchar(2100)")
      odbc::dbWriteTable(con, paste0("PERF_", StandardKey, "PerfStatus"), perf_status, overwrite = TRUE, field.types = sql.field.types)
    } else{
      utils::write.csv(perf_status, save.dir.csv, row.names = FALSE)
      saveRDS(perf_status, save.dir.rds)
    }
  }
 if(DataBase) odbc::dbDisconnect(con)
  invisible(perf_status)

}
