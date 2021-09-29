#' Create Performance graphs
#'
#' This function loads the MPS and OPS data
#' and creates all MPS and OPS performance graphs
#'
#' @param my.dir character
#' @param df.mps dataframe
#' @param df.ops dataframe
#' @param load.data boolean
#' @param output.dir character
#' @param run.parallel boolean
#' @param action.points boolean
#' @param mps.graphs boolean
#' @param ops.graphs boolean
#' @param iprp.graphs boolean
#' @param ops.iprp.graphs boolean
#' @param DataBase boolean
#'
#' @return
#' @export
#'
#' @examples
#'


plot_perf_graphs_all <- function(
  my.dir = getwd(),
  df.mps = NULL,
  df.ops = NULL,
  load.data = TRUE,
  output.dir =
    paste0(
      my.dir,
      "/graphs"
    ),
  run.parallel = FALSE,
  action.points = FALSE,
  mps.graphs = TRUE,
  ops.graphs = TRUE,
  iprp.graphs = TRUE,
  ops.iprp.graphs = TRUE,
  DataBase = TRUE
){

  perf_status_mps <- df.mps
  perf_status_mps <- df.ops

  if(!dir.exists(output.dir)) {
    dir.create(output.dir)
  }


  if(load.data){
    if(DataBase){
      con <- odbc::dbConnect(odbc::odbc(),
                             Driver = "SQL Server",
                             Server = "data-mgmt",
                             Database = "MOSL_Sandpit",
                             Port = 1433,
                             trusted_connection = "True")

      perf_status_mps <- dplyr::tbl(con, "PERF_MPSPERFStatus") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period))
      perf_status_ops <- dplyr::tbl(con, "PERF_OPSPERFStatus") %>%
        dplyr::as_tibble()  %>%
        dplyr::mutate(Period = as.Date(Period),
                      Details = iconv(Details),
                      Context = iconv(Context))
      odbc::dbDisconnect(con)
    }else{
      perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))%>%
        dplyr::filter(TaskVolume > 0)
      perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))%>%
        dplyr::filter(TaskVolume > 0)
    }

  }

  endpoint_url <- "https://stmosldataanalyticswe.blob.core.windows.net/"
  sas <- readr::read_file(ifelse(file.exists(paste0(my.dir, "/data/inputs/digitaldata_sas.txt")), paste0(my.dir, "/data/inputs/digitaldata_sas.txt"), choose.files()))
  bl_endp_key <- AzureStor::storage_endpoint(endpoint = endpoint_url, sas = sas)
  cont <- AzureStor::blob_container(bl_endp_key, "digitaldata")


  if(run.parallel){

    if(!require(doSNOW)){
      install.packages(doSNOW)
    }


    `%dopar%` <- foreach::`%dopar%`
    cores <- parallel::detectCores()
    cl <- snow::makeSOCKcluster(cores-1)
    doSNOW::registerDoSNOW(cl)

    if(mps.graphs){

      if(!dir.exists(paste0(output.dir, "/MPS"))) {
        dir.create(paste0(output.dir, "/MPS"))
      }

      foreach::foreach(
        TP = unique(perf_status_mps$Trading.Party.ID),
        .export = ls(),
        .packages = "tidyverse") %dopar% {

          temp_data_mps <- perf_status_mps%>%
            dplyr::filter(Trading.Party.ID == TP)

          for(standard in unique(temp_data_mps$Standard)){
            plot_data_mps <- temp_data_mps%>%
              dplyr::filter(
                Standard == standard
              )

            on.Rectification <- plot_data_mps$Rectification[plot_data_mps$Period == max(plot_data_mps$Period)]


            tryCatch(
              expr = {
                MOSLR::plot_perf_graph(
                  df = plot_data_mps,
                  trading.party = TP,
                  standard = standard,
                  graph.title = paste(TP, standard),
                  include.iprp = on.Rectification,
                  action.points = action.points
                )+
                  ggplot2::ggsave(file = paste0(output.dir, "/MPS/", paste(TP, standard), ".png"))

                AzureStor::storage_upload(cont,
                                          paste0(output.dir, "/MPS/", paste(TP, standard), ".png"),
                                          paste0("PerfReports/graphs/MPS/", paste(TP, standard), ".png"))
              },
              error = function(e) {
                print(e)
              }
            )
          }
        }

    }




    if(ops.graphs){

      if(!dir.exists(paste0(output.dir, "/OPS"))) {
        dir.create(paste0(output.dir, "/OPS"))
      }

      foreach::foreach(TP = unique(perf_status_ops$Trading.Party.ID),
                       .export = ls(),
                       .packages = "tidyverse") %dopar% {

                         temp_data_ops <- perf_status_ops%>%
                           dplyr::filter(Trading.Party.ID == TP)

                         for(standard in unique(temp_data_ops$Standard)){
                           plot_data_OPS <- temp_data_ops%>%
                             dplyr::filter(
                               Standard == standard
                             )


                           plot_data_ops_KPI <- temp_data_ops%>%
                             dplyr::filter(
                               Standard == standard,
                               PerformanceMeasure == "Completed"
                             )

                           plot_data_ops_API <- temp_data_ops%>%
                             dplyr::filter(
                               Standard == standard,
                               PerformanceMeasure == "Outstanding"
                             )
                           on.iprp <- any(plot_data_OPS$ActiveIPRP[plot_data_OPS$Period == max(plot_data_OPS$Period)])
                           on.iprp.KPI <- plot_data_ops_KPI$ActiveIPRP[plot_data_ops_KPI$Period == max(plot_data_ops_KPI$Period)]
                           on.iprp.API <- plot_data_ops_API$ActiveIPRP[plot_data_ops_API$Period == max(plot_data_ops_API$Period)]


                           tryCatch(
                             expr = {
                               MOSLR::plot_perf_graph(
                                 df = plot_data_ops_KPI,
                                 trading.party = TP,
                                 standard = standard,
                                 graph.title = paste(TP, standard, "KPI"),
                                 include.iprp = on.iprp.KPI,
                                 action.points = action.points
                               )+
                                 ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
                               AzureStor::storage_upload(cont,
                                                         paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"),
                                                         paste0("PerfReports/graphs/OPS/", paste(TP, standard), " KPI.png"))
                             },
                             error = function(e) {
                               print(paste(e,TP,standard,"KPI"))
                             }
                           )

                           tryCatch(
                             expr = {
                               MOSLR::plot_perf_graph(
                                 df = plot_data_ops_API,
                                 trading.party = TP,
                                 standard = standard,
                                 graph.title = paste(TP, standard, "API"),
                                 include.iprp = on.iprp.API,
                                 action.points = action.points
                               )+
                                 ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))

                               AzureStor::storage_upload(cont,
                                                         paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"),
                                                         paste0("PerfReports/graphs/OPS/", paste(TP, standard), " API.png"))
                             },
                             error = function(e) {
                               print(paste(e,TP,standard,"API"))
                             }
                           )


                           tryCatch(
                             expr = {
                               MOSLR::plot_perf_graph(
                                 df = plot_data_OPS,
                                 trading.party = TP,
                                 standard = standard,
                                 graph.title = paste(TP, standard),
                                 include.iprp = on.iprp,
                                 action.points = action.points
                               )+
                                 ggplot2::facet_wrap(~PerformanceMeasure)+
                                 ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), ".png"), width = 12)

                               AzureStor::storage_upload(cont,
                                                         paste0(output.dir, "/OPS/", paste(TP, standard), ".png"),
                                                         paste0("PerfReports/graphs/OPS/", paste(TP, standard), ".png"))
                             },
                             error = function(e) {
                               print(paste(e,TP,standard))
                             }
                           )
                         }
                       }
    }

    if(iprp.graphs){

      if(!dir.exists(paste0(output.dir, "/IPRP"))) {
        dir.create(paste0(output.dir, "/IPRP"))
      }

      iprp_list <- unique(perf_status_mps$key[perf_status_mps$ActiveIPRP == T])

      foreach::foreach(iprp.key = iprp_list,
                       .export = ls(),
                       .packages = "tidyverse") %dopar%{

                         plot_data_iprp <- perf_status_mps%>%dplyr::filter(key == iprp.key)

                         tryCatch(
                           expr = {
                             MOSLR::plot_perf_graph(
                               df = plot_data_iprp,
                               trading.party = plot_data_iprp$Trading.Party.ID[1],
                               standard = plot_data_iprp$Standard[1],
                               graph.title = iprp.key,
                               include.iprp = T,
                               action.points = action.points
                             )+
                               ggplot2::ggsave(file = paste0(output.dir,"/IPRP/", iprp.key, ".png"))

                             AzureStor::storage_upload(cont,
                                                       paste0(output.dir, "/IPRP/", iprp.key, ".png"),
                                                       paste0("PerfReports/graphs/IPRP/", iprp.key, ".png"))
                           },
                           error = function(e) {
                             print(paste(e, iprp.key))
                           }
                         )

                       }

    }

    if(ops.iprp.graphs){

      if(!dir.exists(paste0(output.dir, "/OPS.IPRP"))) {
        dir.create(paste0(output.dir, "/OPS.IPRP"))
      }

      iprp_list <- unique(perf_status_ops$key[perf_status_ops$ActiveIPRP == T])

      foreach::foreach(iprp.key = iprp_list,
                       .export = ls(),
                       .packages = "tidyverse") %dopar%{

                         plot_data_OPS_iprp <- perf_status_ops%>%dplyr::filter(key == iprp.key)
                         TP <- plot_data_OPS_iprp$Trading.Party.ID[1]
                         standard <- plot_data_OPS_iprp$Standard[1]

                         tryCatch(
                           expr = {
                             MOSLR::plot_perf_graph(
                               df = plot_data_OPS_iprp,
                               trading.party = TP ,
                               standard = standard,
                               graph.title = paste(TP, standard),
                               include.iprp = T,
                               action.points = action.points
                             )+
                               ggplot2::facet_wrap(~PerformanceMeasure)+
                               ggplot2::ggsave(file = paste0(output.dir, "/OPS.IPRP/", paste(TP, standard), ".png"), width = 12)

                             AzureStor::storage_upload(cont,
                                                       paste0(output.dir, "/OPS.IPRP/", paste(TP, standard), ".png"),
                                                       paste0("PerfReports/graphs/OPS.IPRP/", paste(TP, standard), ".png"))
                           },
                           error = function(e) {
                             print(paste(e,TP,standard))
                           }
                         )

                       }

    }





    snow::stopCluster(cl)
  } else{

    if(mps.graphs){

      if(!dir.exists(paste0(output.dir, "/MPS"))) {
        dir.create(paste0(output.dir, "/MPS"))
      }

      for(TP in unique(perf_status_mps$Trading.Party.ID)) {

        temp_data_mps <- perf_status_mps%>%
          dplyr::filter(Trading.Party.ID == TP)

        for(standard in unique(temp_data_mps$Standard)){
          plot_data_mps <- temp_data_mps%>%
            dplyr::filter(
              Standard == standard
            )

          on.Rectification <- plot_data_mps$Rectification[plot_data_mps$Period == max(plot_data_mps$Period)]


          tryCatch(
            expr = {
              MOSLR::plot_perf_graph(
                df = plot_data_mps,
                trading.party = TP,
                standard = standard,
                graph.title = paste(TP, standard),
                include.iprp = on.Rectification,
                action.points = action.points
              )+
                ggplot2::ggsave(file = paste0(output.dir,"/MPS/", paste(TP, standard), ".png"))

              AzureStor::storage_upload(cont,
                                        paste0(output.dir, "/MPS/", paste(TP, standard), ".png"),
                                        paste0("PerfReports/graphs/MPS/", paste(TP, standard), ".png"))
            },
            error = function(e) {
              print(paste(e, TP, standard))
            }
          )
        }
      }
    }


    if(ops.graphs){

      if(!dir.exists(paste0(output.dir, "/OPS"))) {
        dir.create(paste0(output.dir, "/OPS"))
      }

      for(TP in unique(perf_status_ops$Trading.Party.ID)) {

        temp_data_ops <- perf_status_ops%>%
          dplyr::filter(Trading.Party.ID == TP)

        for(standard in unique(temp_data_ops$Standard)){

          plot_data_OPS <- temp_data_ops%>%
            dplyr::filter(Standard == standard)

          plot_data_ops_KPI <- temp_data_ops%>%
            dplyr::filter(
              Standard == standard,
              PerformanceMeasure == "Completed"
            )

          plot_data_ops_API <- temp_data_ops%>%
            dplyr::filter(
              Standard == standard,
              PerformanceMeasure == "Outstanding"
            )


          on.iprp <- any(plot_data_OPS$ActiveIPRP[plot_data_OPS$Period == max(plot_data_OPS$Period)])
          on.iprp.KPI <- plot_data_ops_KPI$ActiveIPRP[plot_data_ops_KPI$Period == max(plot_data_ops_KPI$Period)]
          on.iprp.API <- plot_data_ops_API$ActiveIPRP[plot_data_ops_API$Period == max(plot_data_ops_API$Period)]



          tryCatch(
            expr = {
              MOSLR::plot_perf_graph(
                df = plot_data_ops_KPI,
                trading.party = TP,
                standard = standard,
                graph.title = paste(TP, standard, "KPI"),
                include.iprp = on.iprp.KPI,
                action.points = action.points
              )+
                ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
              AzureStor::storage_upload(cont,
                                        paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"),
                                        paste0("PerfReports/graphs/OPS/", paste(TP, standard), " KPI.png"))
            },
            error = function(e) {
              print(paste(e,TP,standard,"KPI"))
            }
          )

          tryCatch(
            expr = {
              MOSLR::plot_perf_graph(
                df = plot_data_ops_API,
                trading.party = TP,
                standard = standard,
                graph.title = paste(TP, standard, "API"),
                include.iprp = on.iprp.API,
                action.points = action.points
              )+
                ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))

              AzureStor::storage_upload(cont,
                                        paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"),
                                        paste0("PerfReports/graphs/OPS/", paste(TP, standard), " API.png"))
            },
            error = function(e) {
              print(paste(e,TP,standard,"API"))
            }
          )

          tryCatch(
            expr = {
              MOSLR::plot_perf_graph(
                df = plot_data_OPS,
                trading.party = TP,
                standard = standard,
                graph.title = paste(TP, standard),
                include.iprp = on.iprp,
                action.points = action.points
              )+
                ggplot2::facet_wrap(~PerformanceMeasure)+
                ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), ".png"), width = 12)

              AzureStor::storage_upload(cont,
                                        paste0(output.dir, "/OPS/", paste(TP, standard), ".png"),
                                        paste0("PerfReports/graphs/OPS/", paste(TP, standard), ".png"))
            },
            error = function(e) {
              print(paste(e,TP,standard))
            }
          )

        }
      }
    }

    if(iprp.graphs){

      if(!dir.exists(paste0(output.dir, "/IPRP"))) {
        dir.create(paste0(output.dir, "/IPRP"))
      }

      iprp_list <- unique(perf_status_mps$key[perf_status_mps$ActiveIPRP == T])

      for(iprp.key in iprp_list){

        plot_data_iprp <- perf_status_mps%>%dplyr::filter(key == iprp.key)

        tryCatch(
          expr = {
            MOSLR::plot_perf_graph(
              df = plot_data_iprp,
              trading.party = plot_data_iprp$Trading.Party.ID[1],
              standard = plot_data_iprp$Standard[1],
              graph.title = iprp.key,
              include.iprp = T,
              action.points = action.points
            )+
              ggplot2::ggsave(file = paste0(output.dir,"/IPRP/", iprp.key, ".png"))

            AzureStor::storage_upload(cont,
                                      paste0(output.dir, "/IPRP/", iprp.key, ".png"),
                                      paste0("PerfReports/graphs/IPRP/", iprp.key, ".png"))
          },
          error = function(e) {
            print(paste(e, iprp.key))
          }
        )

      }

    }

    if(ops.iprp.graphs){

      if(!dir.exists(paste0(output.dir, "/OPS.IPRP"))) {
        dir.create(paste0(output.dir, "/OPS.IPRP"))
      }

      iprp_list <- unique(perf_status_ops$key[perf_status_ops$ActiveIPRP == T])

      for(iprp.key in iprp_list){

        plot_data_OPS_iprp <- perf_status_ops%>%dplyr::filter(key == iprp.key)
        TP <- plot_data_OPS_iprp$Trading.Party.ID[1]
        standard <- plot_data_OPS_iprp$Standard[1]

        tryCatch(
          expr = {
            MOSLR::plot_perf_graph(
              df = plot_data_OPS_iprp,
              trading.party = TP ,
              standard = standard,
              graph.title = paste(TP, standard),
              include.iprp = T,
              action.points = action.points
            )+
              ggplot2::facet_wrap(~PerformanceMeasure)+
              ggplot2::ggsave(file = paste0(output.dir, "/OPS.IPRP/", paste(TP, standard), ".png"), width = 12)

            AzureStor::storage_upload(cont,
                                      paste0(output.dir, "/OPS.IPRP/", paste(TP, standard), ".png"),
                                      paste0("PerfReports/graphs/OPS.IPRP/", paste(TP, standard), ".png"))
          },
          error = function(e) {
            print(paste(e,TP,standard))
          }
        )

      }

    }

  }


}


