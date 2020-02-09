
#' Process Monthly Tracker (Post Analysis and PfM Commentary)
#'
#' This function processes the performance data for each Trading
#' Party by a given standard after the analysis and PfM Commentary
#' has been added. This will then be used to  render the PfM
#' and Monthly Performance Reports created each month.
#'
#' @param save.output  logical
#' @param save.dir.rds character
#' @param save.dir.csv character
#' @param my.dir character
#' @param keep.vars logical
#' @param period.create date
#' @param standard character
#' @param standard.data dataframe
#' @param iprp.plans dataframe
#' @param tracking.sheet dataframe
#'
#' @return
#' @export
#'
#' @examples

process_tracker <- function(
  my.dir = getwd(),
  standard.data,
  iprp.plans,
  tracking.sheet,
  standard,
  save.output = TRUE,
  save.dir.rds = paste0(my.dir, "/data/rdata/perf_status_mps.Rda"),
  save.dir.csv = paste0(my.dir, "/data/outputs/perf_status_mps.csv"),
  keep.vars = TRUE,
  period.create = Sys.Date() %m-% months(1)
  ) {


  # Setting parameters ------------------------------------------------------

  if (keep.vars) {
    var_list <- NULL
  } else {
    var_list <-
      c(
        "Date", "SecondaryCategory", "Trading.Party.ID", "Standard", "PerformanceMeasure",
        "Action", "Rationale", "PFM_Commentary", "PerfFlag3m", "PerfFlag6m",
        "ActiveIPRP", "IPRPend", "MilestoneFlag", "Pending", "UnderReview",
        "OnWatchIPRPend",  "OnWatch", "Consistency", "PerfRating", "IPRPeligible",
        "CumWatch", "CumIPRP", "CumResubmit", "CumEscalate", "CumExtend"
      )
  }


  # Importing data ----------------------------------------------------------

  my_dir <- my.dir

  period.create <- as.Date(period.create)

  monthly_tracking_pre <-
    MOSLR::create_tracker(
      my.dir = my_dir,
      standard.data,
      iprp.plans,
      tracking.sheet,
      standard,
      period = period.create,
      period.only = FALSE,
      save.output = FALSE,
      keep.vars = TRUE,
      filter.category = NULL
      ) %>%
    dplyr::select(
      -Action, - Rationale, -PFM_Commentary
      )

  monthly_tracking_post <- utils::read.csv(paste0(my.dir,"/data/inputs/tracking_", standard, ".csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
      ) %>%
    dplyr::select(
      Date, Trading.Party.ID, Standard, PerformanceMeasure,
      Action, Rationale, PFM_Commentary, Response_Received
      )

  saveRDS(monthly_tracking_post, paste0(my.dir, "/data/rdata/monthly_tracking_", standard, "_post.Rda"))


  # Full Joining IPRP status and mps tracking ----------------------------------

  perf_status <- monthly_tracking_pre %>%
    dplyr::full_join(
      monthly_tracking_post,
      by = c("Date", "Trading.Party.ID", "Standard")
      ) %>%
    dplyr::mutate(
      Action = tolower(Action)
      ) %>%
    dplyr::arrange(Date, Trading.Party.ID, Standard) %>%
    dplyr::group_by(Trading.Party.ID, Standard) %>%
    dplyr::mutate(
      Action = tidyr::replace_na(Action, ""),
      CumWatch = cumsum(OnWatch),
      CumIPRP = cumsum(Action == "iprp"),
      CumResubmit = cumsum(Action == "re-submit"),
      CumEscalate = cumsum(Action == "escalate"),
      CumExtend = cumsum(Action == "extend")
      ) %>%
    dplyr::ungroup() %>%
    {if (!keep.vars) {
      dplyr::select(., var_list)
    } else {
      dplyr::select(., dplyr::everything())
    }
    }

  if(save.output) {
    utils::write.csv(perf_status, save.dir.csv, row.names = FALSE)
    saveRDS(perf_status, save.dir.rds)
  }

  invisible(perf_status)

}
