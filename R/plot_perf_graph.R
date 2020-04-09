
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
#' @param standard.key character
#' @param performance.measure character
#' @param demo.graph logical
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

plot_perf_graph <- function(
  df = NULL,
  my.dir = NULL,
  load.data = FALSE,
  standard.key = NULL,
  performance.measure = NULL,
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
  sub.title = NULL,
  demo.graph = FALSE
  ){

  period.start <- as.Date(period.start)
  period.end <- as.Date(period.end)

  if (demo.graph) {

    x <- 0

    while (x == 0) {

      standard.key <- sample(c("ops", "mps"), 1)

      df <- readRDS(paste0(my.dir, "/data/rdata/perf_status_", tolower(standard.key), ".Rda")) %>%
        tidyr::drop_na(Performance) %>%
        droplevels() %>%
        dplyr::filter(
          StandardKey == toupper(standard.key),
          Standard == sample(as.character(Standard), 1),
          Trading.Party.ID == sample(as.character(Trading.Party.ID), 1),
          PerformanceMeasure == sample(as.character(PerformanceMeasure), 1)
        )

      standard.key <- unique(df$StandardKey)
      standard <- unique(df$Standard)
      trading.party <- unique(df$Trading.Party.ID)
      performance.measure <- unique(df$PerformanceMeasure)
      action.points <- TRUE
      include.iprp <- TRUE

      x <- nrow(df)

    }
  }

  if (is.null(graph.title)) {

    graph_title <- paste0(trading.party, " (", standard, ", ", performance.measure, ")")

    } else {

      graph_title <- graph.title

    }

  size.values <- c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1)
  alpha.manual <- c(0, 0, 0, 1, 1, 0)
  shape.manual <- c(0, 0, 0, 19, 19, 0)
  linetype.values <- c(1, 1, 3, 1, 1, 1)
  colour.values <- c("#425563", "#05C3DE", "#005F83", "#8866BC", "#FFAA4D", "#F9E547", "#8866BC")

  if (load.data & is.null(df)) {
    df <- readRDS(paste0(my.dir, "/data/rdata/perf_status_", tolower(standard.key), ".Rda"))
    }


  graph_data <- df %>%
    dplyr::filter(
      Trading.Party.ID == trading.party,
      Standard == standard,
      Period >= period.start,
      Period <= period.end,
      TaskVolume > 0
      ) %>%
    dplyr::select(
      Period,
      Trading.Party.ID,
      Standard,
      Performance,
      TaskVolume,
      MarketMean,
      TaskShare,
      Planned_Perf,
      PerformanceMeasure,
      Threshold
      ) %>%
    dplyr::arrange(
      Standard, Period
      ) %>%
    tidyr::gather(
      key = "variable",
      value = "value",
      Performance, MarketMean, TaskShare, Threshold, Planned_Perf
      ) %>%
    dplyr::mutate(
      TaskVolume =
        dplyr::if_else (
          variable %in%
            c("MarketMean", "Planned_Perf", "TaskShare"),
          0, as.double(TaskVolume)
        ),
      variable = factor(
        variable,
        levels = c(
          "Performance", "MarketMean", "TaskShare", "Threshold", "Planned_Perf"
          ),
        labels = c(
          "Performance", "Market Mean", "Task Share", "Threshold", "Rectification Milestones"
          )
        )
      ) %>%
    tidyr::drop_na(value) %>%
    droplevels() %>%
    {if (!include.iprp) {
      dplyr::filter(., !variable %in% c("Planned_Perf", "Rectification Milestones"))
      } else {
        dplyr::select(., dplyr::everything())
      }
      } %>%
    {if (!is.null(performance.measure)) {
      dplyr::filter(., PerformanceMeasure == performance.measure)
    } else {
      dplyr::filter(., PerformanceMeasure == PerformanceMeasure)
    }
    }

  graph <- graph_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = Period,
        y = TaskVolume,
        fill = fill.label
        ),
      stat = "identity",
      position = "dodge",
      inherit.aes = FALSE
      ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = Period,
        y = value * max(graph_data$TaskVolume),
        colour = variable,
        linetype = variable,
        size = variable
        )
      ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = Period,
        y = value * max(graph_data$TaskVolume),
        colour = variable,
        shape = variable,
        alpha = variable
        ),
      size = 2
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
      na.value = "1"
      ) +
    ggplot2::scale_linetype_manual(
      values = linetype.values,
      na.value = "1"
      ) +
    ggplot2::scale_colour_manual(
      values = colour.values,
      na.value = "red"
      ) +
    ggplot2::scale_shape_manual(
      values = shape.manual,
      na.value = 0
      ) +
    ggplot2::scale_alpha_manual(
      values = alpha.manual,
      na.value = 0
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
      dplyr::select(Period, Trading.Party.ID, Standard, Action, Performance, PerformanceMeasure) %>%
      dplyr::filter(
        Trading.Party.ID == trading.party,
        Standard == standard,
        Period >= period.start,
        Period <= period.end,
        Action != ""
        )

    graph <- graph +
      ggrepel::geom_label_repel(
        data = actions,
        ggplot2::aes(
          x = Period,
          y = Performance * max(graph_data$TaskVolume),
          label = Action
          ),
        size = 4,
        alpha = 0.7
        ) +
      ggplot2::geom_point(
        data = actions,
        ggplot2::aes(
          x = Period,
          y = Performance * max(graph_data$TaskVolume)
          )
        )
  }


  return(graph)

  }
