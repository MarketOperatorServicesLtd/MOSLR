#' Create Performance graphs
#'
#' This function loads the MPS and OPS data
#' and creates all MPS and OPS performance graphs
#'
#' @param my.dir character
#' @param df dataframe
#' @param load.data boolean
#' @param output.dir character
#' @param run.parallel boolean
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
  run.parallel = FALSE
){

  perf_status_mps <- df.mps
  perf_status_mps <- df.ops


  if(load.data){
    perf_status_mps <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))%>%
      dplyr::filter(TaskVolume > 0)
    perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))%>%
      dplyr::filter(TaskVolume > 0)
  }


  if(run.parallel){

    if(!require(doSNOW)){
      install.packages(doSNOW)
    }


  `%dopar%` <- foreach::`%dopar%`
  cores <- parallel::detectCores()
  cl <- snow::makeCluster(cores-1)
  doSNOW::registerDoSNOW(cl)



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
          action.points = T
          )+
          ggplot2::ggsave(file = paste0(output.dir, "/MPS/", paste(TP, standard), ".png"))
           },
      error = function(e) {
        print(e)
      }
    )
  }
    }



  foreach::foreach(TP = unique(perf_status_ops$Trading.Party.ID),
          .export = ls(),
          .packages = "tidyverse") %dopar% {

    temp_data_ops <- perf_status_ops%>%
      dplyr::filter(Trading.Party.ID == TP)

    for(standard in unique(temp_data_ops$Standard)){
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
            action.points = T
          )+
            ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
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
            action.points = T
          )+
            ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))
        },
        error = function(e) {
          print(paste(e,TP,standard,"API"))
        }
      )
    }
  }

  snow::stopCluster(cl)
  } else{

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
                action.points = T
              )+
                ggplot2::ggsave(file = paste0(output.dir,"/MPS/", paste(TP, standard), ".png"))
            },
            error = function(e) {
              print(paste(e, TP, standard))
            }
          )
        }
    }


    for(TP in unique(perf_status_ops$Trading.Party.ID)) {

      temp_data_ops <- perf_status_ops%>%
        dplyr::filter(Trading.Party.ID == TP)

      for(standard in unique(temp_data_ops$Standard)){
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
              action.points = T
            )+
              ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
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
              action.points = T
              )+
              ggplot2::ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))
          },
          error = function(e) {
            print(paste(e,TP,standard,"API"))
          }
        )
      }
    }

  }


}


