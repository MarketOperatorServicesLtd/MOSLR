

#' Plot Bubble Graph
#'
#' This function creates a bubble graph, based
#' on either long unread data or vacancy data,
#' where on the x-axis is the current vacancy
#' or longunread level and on the y-axis is the
#' change in the longunread or vacancy level
#' compared to the level 12 months ago
#'
#'
#' @param df dataframe
#' @param vertical.sep numerical
#' @param my.dir character
#' @param title character
#' @param subtitle character
#' @param caption character
#' @param x character
#' @param y character
#'
#' @return A bubble graph for MPOP reporting
#' @export
#'
#' @examples


bubble_chart <- function(
  df = NULL,
  vertical.sep = 0.15,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_lab = NULL,
  y_lab = NULL
  )
  {

    arrow = ggplot2::arrow(angle = 10, type = "closed")

    graph <-
      ggplot2::ggplot(df, ggplot2::aes(x = percent, y = change, label = ShortName)) +
      ggplot2::geom_rect(xmin = -Inf, xmax = -vertical.sep - 0.025, ymin = -Inf, ymax = 0, fill = "#F08080", alpha = 0.1) +
      ggplot2::geom_rect(xmin = -Inf, xmax = -vertical.sep - 0.025, ymin = 0, ymax = Inf, fill = "#FFE699", alpha = 0.1) +
      ggplot2::geom_rect(xmin = -vertical.sep - 0.025, xmax = Inf, ymin = -Inf, ymax = 0, fill = "#FFE699", alpha = 0.1) +
      ggplot2::geom_rect(xmin = -vertical.sep - 0.025, xmax = Inf, ymin = 0, ymax = Inf, fill = "#C6E0B4", alpha = 0.1) +
      ggplot2::geom_point(ggplot2::aes(size = TotalPremises, fill = `Change in size`), pch = 21) +
      ggrepel::geom_text_repel(size = 2.5, fontface = "bold") +
      ggplot2::annotate(geom = "label", x = -0.025, y = -max(abs(df$change)), label = "Leading", fill = "azure2") +
      ggplot2::annotate(geom = "label", x = 2 * (vertical.sep + 0.025) + 0.02, y = -max(abs(df$change)), label = "Improving", fill = "azure2") +
      ggplot2::annotate(geom = "label", x = -0.03, y = max(abs(df$change)), label = "At risk", fill = "azure2") +
      ggplot2::annotate(geom = "label", x = 2 * (vertical.sep + 0.025) + 0.023, y = max(abs(df$change)), label = "Declining", fill = "azure2") +
      ggplot2::scale_x_reverse(labels = scales::percent_format(accuracy = 1), limits = c(2 * (vertical.sep + 0.025) + 0.03,-0.03)) +
      ggplot2::scale_y_reverse(labels = scales::percent_format(accuracy = 1), limits = c(max(abs(df$change)), -max(abs(df$change)))) +
      ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = x_lab, y = y_lab) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 9, face = "bold"),
        axis.title = ggplot2::element_text(size = 9, face = "bold"),
        axis.line = ggplot2::element_line(arrow = arrow),
        title = ggplot2::element_text(size = 9, face = "bold"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 12, face = "bold"),
        legend.text = ggplot2::element_text(size = 8)
      ) +
      ggplot2::scale_size_continuous(range = c(1, 20)) +
      ggplot2::scale_fill_manual(values = c("#005F83", "#05C3DE", "#CDCDCD")) +
      ggplot2::guides(size = FALSE, fill = ggplot2::guide_legend(override.aes = list(size = 7)))

      return(graph)

}
