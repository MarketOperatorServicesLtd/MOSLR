
#' Process Monthly OPS Tracker (Post Analysis and PfM Commentary)
#'
#' This function processes the OPS performance data for each
#' Trading Party by OPS after the analysis and PfM Commentary
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
#' @examples

ops_process_tracker <- function(
  my.dir = getwd(),
  save.output = TRUE,
  save.dir.rds = paste0(my.dir, "/data/rdata/perf_status_ops.Rda"),
  save.dir.csv = paste0(my.dir, "/data/outputs/perf_status_ops.csv"),
  keep.vars = TRUE,
  period.create = Sys.Date() %m-% months(1)
) {


  # Setting parameters ------------------------------------------------------

  if (keep.vars) {
    var_list <- NULL
  } else {
    var_list <-
      c(
        "Date", "SecondaryCategory", "Trading.Party.ID", "OPS", "PerformanceMeasure",
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
    MOSLR::ops_create_tracker(
      my.dir = my_dir,
      period = period.create,
      period.only = FALSE,
      save.output = FALSE,
      keep.vars = TRUE,
      filter.category = NULL
      ) %>%
    dplyr::select(
      -Action, - Rationale, -PFM_Commentary, -Response_Received
      )

  monthly_tracking_post <- utils::read.csv(paste0(my.dir,"/data/inputs/tracking_ops.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
      ) %>%
    dplyr::select(
      Date, Trading.Party.ID, OPS, PerformanceMeasure, Action,
      Rationale, PFM_Commentary, Response_Received
      )

  saveRDS(monthly_tracking_post, paste0(my.dir, "/data/rdata/monthly_tracking_ops_post.Rda"))


  # Full Joining IPRP status and ops tracking ----------------------------------

  perf_status_ops <- monthly_tracking_pre %>%
    dplyr::full_join(
      monthly_tracking_post,
      by = c("Date", "Trading.Party.ID", "OPS", "PerformanceMeasure")
      ) %>%
    dplyr::mutate(
      Action = tolower(Action),
      OPS = factor(
        OPS,
        levels = c(
          "OPS A1a", "OPS A2a", "OPS A2b", "OPS A2c", "OPS A3a",
          "OPS A3b", "OPS A4a",
          "OPS B1a", "OPS B3a", "OPS B3b", "OPS B5a", "OPS C1a",
          "OPS C1b", "OPS C2a", "OPS C3a", "OPS C4a", "OPS C4b",
          "OPS C5a", "OPS C6a", "OPS F5a", "OPS F5b", "OPS G2a",
          "OPS G4a", "OPS G4b", "OPS H1a", "OPS I1a", "OPS I1b",
          "OPS I8a", "OPS I8b")
        )
      ) %>%
    dplyr::arrange(Date, Trading.Party.ID, OPS) %>%
    dplyr::group_by(Trading.Party.ID, OPS, PerformanceMeasure) %>%
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
    utils::write.csv(perf_status_ops, save.dir.csv, row.names = FALSE)
    saveRDS(perf_status_ops, save.dir.rds)
  }

  invisible(perf_status_ops)

}
