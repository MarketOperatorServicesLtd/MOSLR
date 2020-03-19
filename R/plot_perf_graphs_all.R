#' Create MPS graphs
#'
#' This function loads the MPS data
#' and creates all MPS performance graphs
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
      filter(TaskVolume > 0)
    perf_status_ops <- readRDS(paste0(my.dir, "/data/rdata/perf_status_ops.Rda"))%>%
      filter(TaskVolume > 0)
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
    filter(Trading.Party.ID == TP)

  for(standard in unique(temp_data_mps$Standard)){
    plot_data_mps <- temp_data_mps%>%
      filter(
        Standard == standard
      )

    on.iprp <- plot_data_mps$ActiveIPRP[plot_data_mps$Period == max(plot_data_mps$Period)]


    tryCatch(
      expr = {
        MOSLR::plot_perf_graph(
          df = plot_data_mps,
          trading.party = TP,
          standard = standard,
          graph.title = paste(TP, standard),
          include.iprp = on.iprp,
          action.points = T
          )+
          ggsave(file = paste0(output.dir, "/MPS/", paste(TP, standard), ".png"))
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
      filter(Trading.Party.ID == TP)

    for(standard in unique(temp_data_ops$Standard)){
      plot_data_ops_KPI <- temp_data_ops%>%
        filter(
          Standard == standard,
          PerformanceMeasure == "Completed"
        )

      plot_data_ops_API <- temp_data_ops%>%
        filter(
          Standard == standard,
          PerformanceMeasure == "Outstanding"
        )

      on.iprp.KPI <- any(plot_data_ops_KPI$ActiveIPRP[plot_data_ops_KPI$Period == max(plot_data_ops_KPI$Period)])
      on.iprp.API <- any(plot_data_ops_API$ActiveIPRP[plot_data_ops_API$Period == max(plot_data_ops_API$Period)])


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
            ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
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
            ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))
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
          filter(Trading.Party.ID == TP)

        for(standard in unique(temp_data_mps$Standard)){
          plot_data_mps <- temp_data_mps%>%
            filter(
              Standard == standard
            )

          on.iprp <- plot_data_mps$ActiveIPRP[plot_data_mps$Period == max(plot_data_mps$Period)]


          tryCatch(
            expr = {
              MOSLR::plot_perf_graph(
                df = plot_data_mps,
                trading.party = TP,
                standard = standard,
                graph.title = paste(TP, standard),
                include.iprp = on.iprp,
                action.points = T
              )+
                ggsave(file = paste0(output.dir,"/MPS/", paste(TP, standard), ".png"))
            },
            error = function(e) {
              print(paste(e, TP, standard))
            }
          )
        }
    }


    for(TP in unique(perf_status_ops$Trading.Party.ID)) {

      temp_data_ops <- perf_status_ops%>%
        filter(Trading.Party.ID == TP)

      for(standard in unique(temp_data_ops$Standard)){
        plot_data_ops_KPI <- temp_data_ops%>%
          filter(
            Standard == standard,
            PerformanceMeasure == "Completed"
          )

        plot_data_ops_API <- temp_data_ops%>%
          filter(
            Standard == standard,
            PerformanceMeasure == "Outstanding"
          )

        on.iprp.KPI <- any(plot_data_ops_KPI$ActiveIPRP[plot_data_ops_KPI$Period == max(plot_data_ops_KPI$Period)])
        on.iprp.API <- any(plot_data_ops_API$ActiveIPRP[plot_data_ops_API$Period == max(plot_data_ops_API$Period)])


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
              ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " KPI.png"))
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
              ggsave(file = paste0(output.dir, "/OPS/", paste(TP, standard), " API.png"))
          },
          error = function(e) {
            print(paste(e,TP,standard,"API"))
          }
        )
      }
    }

  }


}


