
#' Prepare MPS Data for Monthly PfM Report
#'
#' This function prepares the MPS data that will be used
#' to render the PfM Report that is created each month.
#'
#' @param my.dir character
#' @param filter.date.from date
#' @param filter.date.to date
#'
#' @return
#' @export
#'
#' @examples

prep_mps_render_monthly_pfm_report <- function(
  my.dir = getwd(),
  filter.date.from = "2018-04-01",
  filter.date.to = Sys.Date()
  ) {


# Importing data ----------------------------------------------------------

  mps_data_clean <- readRDS(file = paste0(my.dir, "/data/rdata/mps_data_clean.Rda")) %>%
    dplyr::filter(Date >= filter.date.from)

  mps_details <- utils::read.csv(file = paste0(my.dir, "/data/inputs/MPS_details.csv"))

  IPRP_plans_mps <- utils::read.csv(paste0(my.dir, "/data/inputs/IPRP_plans_mps.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      key = as.factor(paste(Trading.Party.ID, MPS))
    )

  IPRP_plan_comparison_mps <- readRDS(paste0(my.dir,"/data/rdata/IPRP_plan_comparison_mps.Rda"))

  tracking_mps <- utils::read.csv(paste0(my.dir,"/data/inputs/tracking_mps.csv")) %>%
    dplyr::mutate(
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Rationale = as.character(Rationale),
      PFM_Commentary = as.character(PFM_Commentary)
    )

  saveRDS(tracking_mps, paste0(my.dir, "/data/rdata/tracking_mps.Rda"))

# Melting MPS and IPRP data join for graphs ---------------------------------------------

  mps_data_melt <- mps_data_clean %>%
    dplyr::left_join(
      IPRP_plans_mps, by = c("Date", "Trading.Party.ID", "MPS", "key")
      ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      MPS,
      key,
      TaskCompletion,
      TaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare,
      Planned_Perf,
      Batch
      ) %>%
    dplyr::filter(TaskVolume > 0) %>%
    tidyr::gather(
      key = "variable",
      value = "value",
      TaskCompletion, MPS_Mean, MPS_Median, TaskShare, Planned_Perf
      ) %>%
    dplyr::mutate(
      TaskVolume =
        dplyr::if_else (
          variable %in%
            c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"),
          0, as.double(TaskVolume)
          ),
      key = as.factor(key),
      variable = factor(variable, levels = c("TaskCompletion", "MPS_Mean", "MPS_Median", "TaskShare", "Planned_Perf"))
      )

  saveRDS(mps_data_melt, paste0(my.dir, "/data/rdata/mps_data_melt.Rda"))


# Full Joining IPRP status and mps tracking ----------------------------------

  perf_status_mps <-
     dplyr::full_join(
       IPRP_plan_comparison_mps,
       tracking_mps,
       by = c("Date", "Trading.Party.ID", "MPS")
       ) %>%
     dplyr::mutate(
       Pending =
         ((Action == "Extend" | Action == "IPRP" | Action == "Resubmit") & Response_Received == ""),
       UnderReview = Action == "Review"
       )

   utils::write.csv(perf_status_mps, paste0(my.dir, "/data/outputs/perf_status_mps.csv"), row.names = FALSE)
   saveRDS(perf_status_mps, paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))

   invisible(perf_status_mps)

}
