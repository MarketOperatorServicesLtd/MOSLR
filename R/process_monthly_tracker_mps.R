
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
#'
#' @return
#' @export
#'
#' @examples

process_monthly_tracker_mps <- function(
  my.dir = getwd(),
  save.output = TRUE,
  save.dir.rds = paste0(my.dir, "/data/rdata/perf_status_mps.Rda"),
  save.dir.csv = paste0(my.dir, "/data/outputs/perf_status_mps.csv")
  ) {


# Importing data ----------------------------------------------------------

  my_dir <- my.dir

  monthly_tracking_pre <-
    MOSLR::create_monthly_tracker(
      my.dir = my_dir, period.only = FALSE, save.output = FALSE
      ) %>%
    dplyr::select(
      -Action, - Rationale, -PFM_Commentary
      )

  monthly_tracking_post <- utils::read.csv(paste0(my.dir,"/data/inputs/tracking_mps.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
    ) %>%
    dplyr::select(
      Date, Trading.Party.ID, MPS, Action, Rationale, PFM_Commentary, Response_Received
    )

  saveRDS(monthly_tracking_post, paste0(my.dir, "/data/rdata/monthly_tracking_mps_post.Rda"))


# Full Joining IPRP status and mps tracking ----------------------------------

  perf_status_mps <- monthly_tracking_pre %>%
    dplyr::full_join(
      monthly_tracking_post,
      by = c("Date", "Trading.Party.ID", "MPS")
      ) %>%
    dplyr::mutate(
      Pending =
        ((Action == "Extend" | Action == "IPRP" | Action == "Resubmit") & Response_Received == ""),
      UnderReview = Action == "Review"
      ) %>%
    dplyr::select(
      Date, Category, Trading.Party.ID, MPS,
      Action, Rationale, PFM_Commentary,
      PerfFlag3m, PerfFlag6m, ActiveIPRP,
      MilestoneFlag, Pending, UnderReview,
      IPRPend, OnWatchIPRPend, OnWatch,
      Consistency, PerfRating
      )

  if(save.output) {
  utils::write.csv(perf_status_mps, save.dir.csv, row.names = FALSE)
  saveRDS(perf_status_mps, save.dir.rds)
  }

  invisible(perf_status_mps)

}
