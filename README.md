
# MOSLR

<!-- badges: start -->
<!-- badges: end -->

The MOSLR package is intended for: 

* The automated generation of PDF performance reports (so-called "PFM Reports") each month 
* To facilitate MOSL's Performance Rectification process by producing required performance statistics and tracking decisions as part of Initial Performance Rectification Plans (IPRP) and Performance Rectification Plans (PRPs)
* Storing several useful functions for various actions, such as adding MOSL brand colours to charts

## Installation

You can install the released version of MOSLR from [GitHub](https://github.com) with:

``` r
devtools::install_github("MarketOperatorServicesLtd/MOSLR")
```

## Example

This is an example script showing how to run the monthly performance reporting and produce the PFM reports:

``` r

library(MOSLR)

tinytex.loc <- "F:\\TinyTex"
my.dir <- "F:\\Market Performance\\MOSLR"
conf.loc <- "F:\\GitAnalysisRepo\\config.yml"

if(!tinytex::is_tinytex()) {
  tinytex::use_tinytex(tinytex.loc)
}

# Market Performance Standards (MPS) ---------------------------------------------------------------------

mps_data <- MOSLR::mps_data_prep(my.dir = my.dir, conf.loc = conf.loc)
mps_tracking <- MOSLR::mps_create_tracker(my.dir = my.dir, conf.loc = conf.loc) 
mps_perf_status <- MOSLR::mps_process_tracker(my.dir = my.dir, conf.loc = conf.loc)

# Additional Performance Indicators (APIs) ---------------------------------------------------------------------

API_tracking <- MOSLR::mps_create_tracker(my.dir = my.dir, StandardKey = "API", conf.loc = conf.loc) 
API_perf_status <- MOSLR::mps_process_tracker(my.dir = my.dir, StandardKey = "API", conf.loc = conf.loc)

# Operational Performance Standards (OPS) ---------------------------------------------------------------------

ops_data <- MOSLR::ops_data_prep(my.dir = my.dir, conf.loc = conf.loc)
ops_tracking <- MOSLR::ops_create_tracker(my.dir = my.dir, conf.loc = conf.loc)
ops_perf_status <- MOSLR::ops_process_tracker(my.dir = my.dir, conf.loc = conf.loc)

# Performance Graphs

plot_perf_graphs_all(my.dir = my.dir, conf.loc = conf.loc, run.parallel = FALSE, mps.graphs = FALSE, ops.graphs = FALSE, iprp.graphs = FALSE)

# Render PFM Report (where period = month the data relates to) ### Run for one Trading Party Only

render_PFM_report(my.dir = my.dir, conf.loc = conf.loc, prep.data = FALSE, data.period = format(Sys.Date(), "%Y-%m-01"), tp.list = c("THAMES-W", "CASTLE-R"))

# Render ALL PFM Reports (where period = month the data relates to)

render_PFM_report(my.dir = my.dir, prep.data = FALSE, data.period = format(Sys.Date(), "%Y-%m-01"))

```

For further details contact the Data Team. 
