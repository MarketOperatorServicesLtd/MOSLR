
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
#' @param df.longunread dataframe
#' @param df.vacancy dataframe
#' @param tp.details dataframe
#' @param longunread.graph boolean
#' @param vacancy.graph boolean
#' @param by character, one of c("Retailer", "Wholesaler", "Pair")
#' @param my.dir character
#' @param load.data boolean
#'
#' @return A bubble graph for MPOP reporting
#' @export
#'
#' @examples



bubble_chart <- function(
  df.longunread = NULL,
  df.vacancy = NULL,
  tp.details = NULL,
  longunread.graph = F,
  vacancy.graph = F,
  by = "Retailer",
  my.dir = getwd(),
  load.data = F
){

  if(!by %in% c("Retailer", "Wholesaler", "Pair")){
    stop("Variable 'by' in bubble_chart has to be one of c('Retailer', 'Wholesaler', 'Pair')")
  }

  longunread <- df.longunread
  vacant <- df.vacancy
  tp_details <- tp.details
  vac_graph <- NULL
  long_graph <- NULL

  # Vacancy  ----------------------------------------------------------------



  if(vacancy.graph){
    if(load.data){

      tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))%>%
        dplyr::mutate(
          TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
          ShortName = stringr::str_replace_all(ShortName, "&", "And"))

      vacant <- utils::read.csv(paste0(my.dir, "/data/inputs/Vacancy_Pairing.csv"))%>%
        dplyr::group_by(
          WholesalerID, Period
        )%>%
        dplyr::mutate(
          whole_vacant = sum(Premises)
        )%>%
        dplyr::ungroup()%>%
        dplyr::group_by(
          RetailerID, Period)%>%
        dplyr::mutate(
          ret_vacant = sum(Premises)
        )%>%
        dplyr::ungroup()%>%
        dplyr::filter(
          whole_vacant > 200,
          ret_vacant > 200
        )%>%
        dplyr::mutate(
          Period = as.Date(Period)
        )%>%
        dplyr::left_join(
          .,
          tp_details,
          by = c("RetailerID" = "Trading.Party.ID"))%>%
        dplyr::rename(
          Retailer = TradingPartyName,
          ShortRet = ShortName
        )%>%
        dplyr::left_join(
          .,
          tp_details,
          by = c("WholesalerID" = "Trading.Party.ID"))%>%
        dplyr::rename(
          Wholesaler = TradingPartyName,
          ShortWhole = ShortName
        )%>%
        dplyr::mutate(
          Pair = stringr::str_replace_all(paste(ShortRet, "+", ShortWhole), "&", "And"),
          Retailer = stringr::str_replace_all(Retailer, "&", "And"),
          Wholesaler = stringr::str_replace_all(Wholesaler, "&", "And"))

      vacant_TP <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_TP.csv"))%>%
        dplyr::mutate(
          percent = paste(round(100*(Vacant_Premises)/(Premises),1),"%"),
          Period = as.Date(Period))%>%
        dplyr::left_join(
          ., tp_details,
          by = c("TradingPartyID" = "Trading.Party.ID")
        )

      vacant_Total <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_Total.csv"))
    }else{
      vacant_TP <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_TP.csv"))%>%
        dplyr::mutate(percent = paste(round(100*(Vacant_Premises)/(Premises),1),"%"))%>%
        dplyr::left_join(
          ., tp_details,
          by = c("TradingPartyID" = "Trading.Party.ID")
        )

      vacant_Total <- read.csv(paste0(my.dir, "/data/inputs/Vacancy_Total.csv"))
    }


# Prep vacancy data -------------------------------------------------------

    vac_MO_avg <- vacant%>%
      dplyr::filter(
        Period=="2017-04-01"
      )%>%
      dplyr::summarise(
        Total = sum(Vacancies)/sum(Premises)
      )


    last12_vac <- vacant%>%
      dplyr::filter(
        Period == max(vacant$Period)%m-%months(12)
      )%>%
      dplyr::group_by_at(
          .vars=by
      )%>%
      dplyr::summarise(
        april_percent = sum(Vacancies)/sum(Premises),
        Total_Premises_april = sum(Premises)
      )

    last1_vac <- vacant%>%
      dplyr::filter(
        Period==max(vacant$Period)%m-%months(1)
      )%>%
      dplyr::group_by_at(
        .vars=by
      )%>%
      dplyr::summarise(
        last_percent = sum(Vacancies)/sum(Premises),
      )

    if(by %in% c("Retailer", "Wholesaler")){
      main_vac <- vacant_TP%>%
        dplyr::filter(
          stringr::str_sub(TradingPartyID, -2) == paste0("-",substr(by, 1,1)),
          Premises > 200
        )%>%
        dplyr::rename_at(vars(TradingPartyName), funs(paste0(by)))%>%
        dplyr::rename(
          TotalPremises = Premises,
          Vacancies = Vacant_Premises)%>%
        dplyr::mutate(
        percent = Vacancies/TotalPremises
      )

    }else{
      main_vac <- vacant%>%
        dplyr::filter(Period == max(vacant$Period))%>%
        dplyr::group_by(Pair)%>%
        dplyr::summarise(
          percent = sum(Vacancies)/sum(Premises),
          TotalPremises = sum(Premises),
          Vacancies = sum(Vacancies)
        )%>%
        dplyr::filter(
          TotalPremises > 6000
        )%>%
        dplyr::mutate(
          ShortName = Pair
        )
    }



      current_vac <<- main_vac%>%
        dplyr::left_join(
        last12_vac,
        by=by
      )%>%
        dplyr::mutate(
        change = percent-april_percent,
        "Change in size" = factor(case_when(
          (TotalPremises - Total_Premises_april)/Total_Premises_april>0.2 ~ "Number of premises increased by 20%",
          (TotalPremises - Total_Premises_april)/Total_Premises_april<0.2&(TotalPremises - Total_Premises_april)/Total_Premises_april>-0.2 ~ "Number of premises within +/- 20%",
          (TotalPremises - Total_Premises_april)/Total_Premises_april< -0.2 ~ "Number of premises reduced by > 20%",
          TRUE ~ "other"
        ),
        levels= c("Number of premises increased by 20%", "Number of premises within +/- 20%", "Number of premises reduced by > 20%")
        )
      )%>%
        dplyr::arrange(
        desc(TotalPremises)
      )%>%
        dplyr::filter(
        !is.na(Total_Premises_april)
      )%>%
        dplyr::left_join(
        last1_vac,
        by = by
      )

    vac_axis <- current_vac%>%
      dplyr::filter(change < 0.15, change > -0.15)

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
      current_vac,
      ggplot2::aes(
        x = percent,
        y = change,
        label = ShortName
      )
    )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -vac_MO_avg$Total-0.025,
        ymin = -Inf,
        ymax = 0,
        fill = "#F08080",
        alpha = 0.1
      )+
      ggplot2:: geom_rect(
        xmin = -Inf,
        xmax = -vac_MO_avg$Total-0.025,
        ymin = 0,
        ymax = Inf,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -vac_MO_avg$Total-0.025,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -vac_MO_avg$Total-0.025,
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
        y = -max(abs(vac_axis$change)),
        label = "Leading",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = 2*(vac_MO_avg$Total+0.025)+0.02,
        y = -max(abs(vac_axis$change)),
        label = "Improving",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.03,
        y = max(abs(vac_axis$change)),
        label = "At risk",
        fill = "azure2")+
      ggplot2::annotate(
        geom="label",
        x = 2*(vac_MO_avg$Total+0.025)+0.023,
        y = max(abs(vac_axis$change)),
        label = "Declining",
        fill = "azure2"
      )+
      ggplot2::scale_x_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(2*(vac_MO_avg$Total+0.025)+0.03,-0.03)
      )+
      ggplot2::scale_y_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(max(abs(vac_axis$change)),-max(abs(vac_axis$change)))
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

    if(load.data){
      tp_details <- utils::read.csv(paste0(my.dir, "/data/inputs/tp_details.csv"))%>%
        dplyr::mutate(
          TradingPartyName = stringr::str_replace_all(TradingPartyName, "&", "And"),
          ShortName = stringr::str_replace_all(ShortName, "&", "And"))

      longunread <- utils::read.csv(paste0(my.dir, "/data/inputs/longunread.csv"))%>%
        dplyr::mutate(
          Period =
            as.Date(paste0(Period, "-01"), "%Y-%m-%d")
        )%>%
        dplyr::group_by(
          WholesalerID, Period
        )%>%
        dplyr::mutate(
          whole_meters = sum(Total_Meters)
        )%>%
        dplyr::ungroup()%>%
        dplyr::group_by(
          RetailerID, Period
        )%>%
        dplyr::mutate(
          ret_meters = sum(Total_Meters)
        )%>%
        dplyr::ungroup()%>%
        dplyr::filter(
          whole_meters > 100,
          ret_meters > 100)%>%
        dplyr::left_join(
          .,
          tp_details,
          by = c("RetailerID" = "Trading.Party.ID"))%>%
        dplyr::rename(
          Retailer = TradingPartyName,
          ShortRet = ShortName
        )%>%
        dplyr::left_join(
          .,
          tp_details,
          by = c("WholesalerID" = "Trading.Party.ID"))%>%
        dplyr::rename(
          Wholesaler = TradingPartyName,
          ShortWhole = ShortName
        )%>%
        dplyr::mutate(Pair = paste(ShortRet, "+", ShortWhole))

    }


# Prep longunread data ----------------------------------------------------


    last12_long <- longunread%>%
      dplyr::filter(
        Period == max(longunread$Period)%m-%months(12)
      )%>%
      dplyr::group_by_at(
        .vars=by
      )%>%
      dplyr::summarise(
        april_percent = sum(Meters_Unread_in_12mo)/sum(Total_Meters),
        Total_Meters_april = sum(Total_Meters)
      )%>%
      dplyr::filter(
        Total_Meters_april > 100
      )%>%
      dplyr::select(
        by,
        Total_Meters_april,
        april_percent)


    last1_long <- longunread%>%
      dplyr::filter(
        Period==max(longunread$Period)%m-%months(1)
      )%>%
      dplyr::group_by_at(
        .vars=by
      )%>%
      dplyr::summarise(
        last_percent = sum(Meters_Unread_in_12mo)/sum(Total_Meters)
      )%>%
      dplyr::select(by, last_percent)

    current_long <<- longunread%>%
      dplyr::filter(
        Period == max(longunread$Period),
        !sub_type.x %in% c("NAV", "Self-supply")
      )%>%{
        if(by == "Retailer"){
          dplyr::group_by(.,
            Retailer,
            ShortRet
          )
        } else if(by == "Wholesaler"){
          dplyr::group_by(.,
        Wholesaler,
        ShortWhole
      )
        } else{
          dplyr::group_by(.,
            Pair)
        }
      }%>%
      dplyr::summarise(
        percent = sum(Meters_Unread_in_12mo)/sum(Total_Meters),
        TotalMeters = sum(Total_Meters),
        PureLUMs = sum(Meters_Unread_in_12mo)
      )%>%
      dplyr::left_join(
        .,
        last12_long,
        by=by
      )%>%
      dplyr::mutate(
        change = percent-april_percent,
        "Change in size" = case_when(
          (TotalMeters - Total_Meters_april)/Total_Meters_april>0.2 ~
            "Meter numbers increased by 20%",
          (TotalMeters - Total_Meters_april)/Total_Meters_april<0.2&
            (TotalMeters - Total_Meters_april)/Total_Meters_april>-0.2 ~
            "Meter number within +/- 20%",
          (TotalMeters - Total_Meters_april)/Total_Meters_april< -0.2 ~
            "Meter numbers reduced by > 20%",
          TRUE ~ "other")
      )%>%
      dplyr::arrange(desc(TotalMeters))%>%
      dplyr::filter(
        !is.na(Total_Meters_april)
      )%>%
      dplyr::left_join(
        ., last1_long,
        by = by
      )%>%{
        if(by == "Retailer"){
          dplyr::rename(., ShortName = ShortRet)
        } else if(by == "Wholesaler"){
          dplyr::rename(., ShortName = ShortWhole)
        } else{
          dplyr::mutate(., ShortName = Pair)%>%
            dplyr::filter(., TotalMeters > 3000)
        }
      }

    axis_long <- current_long%>%
      dplyr::filter(change < 0.15, change >-0.15)

    longunread_avg <- longunread%>%
      dplyr::filter(Period == max(longunread$Period))%>%
      dplyr::summarise(Total = sum(Meters_Unread_in_12mo)/sum(Total_Meters))



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
      current_long,
      ggplot2::aes(
        x = percent,
        y = change,
        label = ShortName
      )
    )+
      ggplot2::geom_rect(
        xmin = -longunread_avg$Total,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = "#C6E0B4",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -longunread_avg$Total,
        ymin = 0,
        ymax = Inf,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -longunread_avg$Total,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = "#FFE699",
        alpha = 0.1
      )+
      ggplot2::geom_rect(
        xmin = -Inf,
        xmax = -longunread_avg$Total,
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
        y = -max(abs(axis_long$change)),
        label = "Leading",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = 2*(longunread_avg$Total+0.015),
        y = -max(abs(axis_long$change)),
        label = "Improving",
        fill = "azure2"
      )+
      ggplot2::annotate(
        geom="label",
        x = -0.04,
        y = max(abs(axis_long$change)),
        label = "At risk",
        fill = "azure2")+
      ggplot2::annotate(
        geom="label",
        x = 2*(longunread_avg$Total+0.015),
        y = max(abs(axis_long$change)),
        label = "Declining",
        fill = "azure2"
      )+
      ggplot2::scale_x_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(2*(longunread_avg$Total+0.02),-0.04)
      )+
      ggplot2::scale_y_reverse(
        labels = scales::percent_format(accuracy = 1),
        limits = c(max(abs(axis_long$change)),-max(abs(axis_long$change)))
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





























