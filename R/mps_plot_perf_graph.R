
#' Plot Performance Graph (MPS)
#'
#' This function produces a grasph with two #' independent
#' axis. The left-handside corresponds to a single variable
#' plotted as a bar, and the right-handside corresponds to
#' multiple lines.
#'
#' @param include.iprp logical
#' @param sec.axis.name character
#' @param fill.manual character
#' @param y.lab character
#' @param x.lab chracter
#' @param graph.title character
#' @param fill.label character
#' @param sub.title character
#' @param period.start date
#' @param period.end date
#' @param action.points logical
#' @param trading.party character
#' @param standard character
#' @param df dataframe
#' @param my.dir character
#' @param load.data logical
#'
#' @return
#' @export
#'
#' @examples

mps_plot_perf_graph <- function(
  df,
  load.data = FALSE,
  my.dir = NULL,
  include.iprp = FALSE,
  trading.party,
  standard,
  period.start = "2018-04-01",
  period.end = Sys.Date(),
  action.points = FALSE,
  sec.axis.name = "Performance",
  fill.label = "Task Volume",
  fill.manual = "azure3",
  y.lab = "Tasks",
  x.lab = "",
  graph.title = NULL,
  sub.title = NULL
  ){

  period.start <- as.Date(period.start)
  period.end <- as.Date(period.end)

  if (is.null(graph.title)) {

    graph_title <- paste0(trading.party, " (", standard, ")")

    } else {

      graph_title <- graph.title

    }


  if (include.iprp) {

    size.values <- c(1, 0.5, 0.5, 0.5, 1)
    alpha.manual <- c(0, 0, 0, 0, 1)
    shape.manual <- c(0, 0, 0, 0, 1)
    linetype.values <- c(1, 2, 1, 3, 1)
    colour.values <- c("darkorange", "azure4", "dodgerblue4", "grey3", "red")
    labels <- c(
      "On-Time Tasks", "Market Mean", "Market Median", "Task Share", "Milestones"
      )

  } else {

    size.values <- c(1, 0.5, 0.5, 0.5)
    alpha.manual <- c(0, 0, 0, 0)
    shape.manual <- c(0, 0, 0, 0)
    linetype.values <- c(1, 2, 1, 3)
    colour.values <- c("darkorange", "azure4", "dodgerblue4", "grey3")
    labels <- c(
      "On-Time Tasks", "Market Mean", "Market Median", "Task Share"
      )

    }

if (load.data & is.null(df)) {

  df <- readRDS(paste0(my.dir, "/data/rdata/perf_status_mps.Rda"))

}


  graph_data <- df %>%
    dplyr::filter(
      Trading.Party.ID == trading.party,
      Standard == standard,
      Date >= period.start,
      Date <= period.end,
      TaskVolume > 0
      ) %>%
    dplyr::select(
      Date,
      Trading.Party.ID,
      Standard,
      OnTimeTaskCompletion,
      TaskVolume,
      MPS_Mean,
      MPS_Median,
      TaskShare,
      Planned_Perf
      ) %>%
    dplyr::arrange(
      Standard, Date
      ) %>%
    tidyr::gather(
      key = "variable",
      value = "value",
      OnTimeTaskCompletion, MPS_Mean, MPS_Median, TaskShare, Planned_Perf
      ) %>%
    dplyr::mutate(
      TaskVolume =
        dplyr::if_else (
          variable %in%
            c("MPS_Mean", "MPS_Median", "Planned_Perf", "TaskShare"),
          0, as.double(TaskVolume)
        ),
      variable = factor(
        variable,
        levels = c(
          "OnTimeTaskCompletion", "MPS_Mean", "MPS_Median", "TaskShare", "Planned_Perf"
          )
        )
      ) %>%
    tidyr::drop_na(value) %>%
    droplevels() %>%
    {if (!include.iprp) {
      dplyr::filter(., variable != "Planned_Perf")
      } else {
        dplyr::select(., dplyr::everything())
      }
      }

  graph <- graph_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = Date,
        y = TaskVolume,
        fill = fill.label
        ),
      stat = "identity",
      position = "dodge",
      inherit.aes = FALSE
      ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = Date,
        y = value * max(graph_data$TaskVolume),
        colour = variable,
        linetype = variable,
        size = variable
        )
      ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = Date,
        y = value * max(graph_data$TaskVolume),
        shape = variable,
        alpha = variable
        )
      ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      breaks = scales::pretty_breaks(4),
      sec.axis =
        ggplot2::sec_axis(
          ~. / max(graph_data$TaskVolume),
          name = sec.axis.name,
          labels = scales::percent_format(accuracy = 1)
          )
      ) +
    ggplot2::scale_fill_manual(
      values = fill.manual,
      na.value = "red"
      ) +
    ggplot2::scale_size_manual(
      values = size.values,
      na.value = "1",
      labels = labels
      ) +
    ggplot2::scale_linetype_manual(
      values = linetype.values,
      na.value = "1",
      labels = labels
      ) +
    ggplot2::scale_colour_manual(
      values = colour.values,
      na.value = "red",
      labels = labels
      ) +
    ggplot2::scale_shape_manual(
      values = shape.manual,
      na.value = 0,
      labels = labels
      ) +
    ggplot2::scale_alpha_manual(
      values = alpha.manual,
      na.value = 0,
      labels = labels
      ) +
    ggplot2::ylab(y.lab) +
    ggplot2::xlab(x.lab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
      ) +
    ggplot2::labs(
      title = graph_title,
      subtitle = sub.title
    )


  if (action.points) {

    actions <- df %>%
      dplyr::select(Date, Trading.Party.ID, Standard, Action, OnTimeTaskCompletion) %>%
      dplyr::filter(
        Trading.Party.ID == trading.party,
        Standard == standard,
        Date >= period.start,
        Date <= period.end,
        Action != ""
        )

    graph <- graph +
      ggrepel::geom_label_repel(
        data = actions,
        ggplot2::aes(
          x = Date,
          y = OnTimeTaskCompletion * max(graph_data$TaskVolume),
          label = Action
          ),
        size = 4,
        alpha = 0.7
        ) +
      ggplot2::geom_point(
        data = actions,
        ggplot2::aes(
          x = Date,
          y = OnTimeTaskCompletion * max(graph_data$TaskVolume)
          )
        )
  }


  return(graph)

  }
