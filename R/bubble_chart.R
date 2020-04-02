
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
#' @param longunread.graph boolean
#' @param vacancy.graph boolean
#' @param by character, one of c("Retailer", "Wholesaler", "Pair")
#' @param df dataframe
#' @param prep.data boolean
#' @param vertical.sep numerical
#' @param my.dir character
#'
#' @return A bubble graph for MPOP reporting
#' @export
#'
#' @examples



bubble_chart <- function(
  df = NULL,
  longunread.graph = F,
  vacancy.graph = F,
  by = "Retailer",
  my.dir = getwd(),
  prep.data = F,
  vertical.sep = 0.15
){

  if(!by %in% c("Retailer", "Wholesaler", "Pair")){
    stop("Variable 'by' in bubble_chart has to be one of c('Retailer', 'Wholesaler', 'Pair')")
  }

  my_dir <- my.dir


  # Vacancy  ----------------------------------------------------------------



  if(vacancy.graph){
    if(prep.data){

      main<-MOSLR::MPOP_data_prep(vacancy.table = T, load.data = T, by = by, my.dir = my_dir)%>%
      dplyr::filter(change < 0.15, change > -0.15)
    }else{
      main <- df%>%
        dplyr::filter(change < 0.15, change > -0.15)
    }


# Prep vacancy data -------------------------------------------------------




    arrow = ggplot2::arrow(angle = 10, type = "closed")

    if(by == "Wholesaler"){
      title <- "Wholesaler area Vacant premises change in the last 12 months"
    }else if(by == "Pair")  {
      title <- "Vacant Premises change in the last 12 months per Pairing"
    }else{
      title <- paste(by, "Vacant premises change in the last 12 months")
    }


# Vacancy plot ------------------------------------------------------------



   graph <- ggplot2::ggplot(
      main,
      ggplot2::aes(
        x = percent,
        y = change,
        label = ShortName
      )
    )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -vertical.sep-0.025,
        ymin = -Inf,
        ymax = 0,
        fill = "#F08080",
        alpha = 0.1
      )+
      ggplot2:: geom_rect(
        xmin = -Inf,
        xmax = -vertical.sep-0.025,
        ymin = 0,
        ymax = Inf,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -vertical.sep-0.025,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -vertical.sep-0.025,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = "#C6E0B4",
        alpha = 0.1
      )+
      ggplot2::geom_point(
        ggplot2::aes(
          size = TotalPremises,
          fill = `Change in size`
        ),
        pch = 21
      )+
      ggrepel::geom_text_repel(
        size = 2.5,
        fontface = "bold"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.025,
        y = -max(abs(main$change)),
        label = "Leading",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = 2*(vertical.sep+0.025)+0.02,
        y = -max(abs(main$change)),
        label = "Improving",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.03,
        y = max(abs(main$change)),
        label = "At risk",
        fill = "azure2")+
      ggplot2::annotate(
        geom="label",
        x = 2*(vertical.sep+0.025)+0.023,
        y = max(abs(main$change)),
        label = "Declining",
        fill = "azure2"
      )+
      ggplot2::scale_x_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(2*(vertical.sep+0.025)+0.03,-0.03)
      )+
      ggplot2::scale_y_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(max(abs(main$change)),-max(abs(main$change)))
      )+
      ggplot2::xlab(
        "Total % of vacant premises"
      )+
      ggplot2::ylab(
        "Change in % of vacant premises in the last 12 months"
      )+
      ggplot2::ggtitle(
        title
      )+
      ggplot2::theme_classic()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(
          size = 9,
          face = "bold"
        ),
        axis.title = ggplot2::element_text(
          size = 9,
          face = "bold"),
        axis.line = ggplot2::element_line(
          arrow = arrow
        ),
        title = ggplot2::element_text(
          size = 9,
          face = "bold"
        ),
        plot.title = ggplot2::element_text(
          hjust = 0.5
        ),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(
          size = 12,
          face = "bold"
        ),
        legend.text = ggplot2::element_text(
          size = 8
        )
      )+
      ggplot2::scale_size_continuous(
        range = c(1,20)
      )+
      ggplot2::scale_fill_manual(
        values = c("#005F83", "#05C3DE", "#CDCDCD")
      )+
      ggplot2::guides(
        size = FALSE,
        fill = ggplot2::guide_legend(
          override.aes = list(size = 7)
        )
      )

  } else if(longunread.graph){

    if(prep.data){

      main <- MOSLR::MPOP_data_prep(longunread.table = T, load.data = T,by = by, my.dir = my_dir)%>%
        dplyr::filter(change < 0.15, change >-0.15)

    }else{
      main <- df%>%
        dplyr::filter(change < 0.15, change >-0.15)
    }





# Longunread plot ---------------------------------------------------------

    arrow = ggplot2::arrow(angle = 10, type = "closed")

    if(by == "Wholesaler"){
      title <- "Wholesaler area Long Unread Meter change in the last 12 months"
    }else if(by == "Pair")  {
      title <- "Long Unread Meter change in the last 12 months per Pairing"
    }else{
      title <- "Retailer Long Unread Meter change in the last 12 months"
    }

    graph <- ggplot2::ggplot(
      main,
      ggplot2::aes(
        x = percent,
        y = change,
        label = ShortName
      )
    )+
      ggplot2::geom_rect(
        xmin = -vertical.sep,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = "#C6E0B4",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -vertical.sep,
        ymin = 0,
        ymax = Inf,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -vertical.sep,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -vertical.sep,
        ymin = -Inf,
        ymax = 0,
        fill = "#F08080",
        alpha = 0.1
      )+
      ggplot2::geom_point(
        ggplot2::aes(
          size = TotalMeters,
          fill = `Change in size`
        ),
        pch = 21
      )+
      ggrepel::geom_text_repel(
        size = 2.5,
        fontface = "bold"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.035,
        y = -max(abs(main$change)),
        label = "Leading",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = 2*(vertical.sep+0.015),
        y = -max(abs(main$change)),
        label = "Improving",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.04,
        y = max(abs(main$change)),
        label = "At risk",
        fill = "azure2")+
      ggplot2::annotate(
        geom="label",
        x = 2*(vertical.sep+0.015),
        y = max(abs(main$change)),
        label = "Declining",
        fill = "azure2"
      )+
      ggplot2::scale_x_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(2*(vertical.sep+0.02),-0.04)
      )+
      ggplot2::scale_y_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(max(abs(main$change)),-max(abs(main$change)))
      )+
      ggplot2::xlab(
        "Total % of meters not read within past 12 months"
      )+
      ggplot2::ylab(
        "Change in % of unread meters in the last 12 months"
      )+
      ggplot2::ggtitle(title)+
      ggplot2::theme_classic()+
      ggplot2::theme(
        axis.text = ggplot2::element_text(
          size = 9,
          face = "bold"
        ),
        axis.title = ggplot2::element_text(
          size = 9,
          face = "bold"),
        axis.line = ggplot2::element_line(
          arrow = arrow
        ),
        title = ggplot2::element_text(
          size = 9,
          face = "bold"
        ),
        plot.title = ggplot2::element_text(
          hjust = 0.5
        ),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(
          size = 12,
          face = "bold"
        ),
        legend.text = ggplot2::element_text(
          size = 8
        )
      )+
      ggplot2::scale_size_continuous(
        range = c(2,20)
      )+
      ggplot2::scale_fill_manual(
        values = c("#05C3DE", "#005F83","#CDCDCD")
      )+
      ggplot2::guides(
        size = FALSE,
        fill = ggplot2::guide_legend(
          override.aes = list(size = 7)
        )
      )

  }
return(graph)
}





























