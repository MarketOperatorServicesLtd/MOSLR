

#' scale_fill_MOSL
#'
#' This function offers a MOSL fill scale for ggplot2
#'
#' @param colours character
#' @param reverse boolean
#' @param palette character
#' @param discrete boolean
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @details
#' @param palette
#' see \code{\link{MOSL_palettes}} for all available colour palettes
#' @param colours
#' Specifies the MOSL colours when palette is NA. When not specified, all colours are selected. See \code{\link{MOSL_colours}} for all available colours.
#' @param reverse
#' Boolean variable to indicate whether to reverse the order of the scale or not
#' @param discrete
#' Boolean variable to indicate whether to use a discrete scale or not


scale_fill_MOSL <- function(colours = NA, reverse = FALSE, palette = NA, discrete = TRUE, ...) {
  suppressWarnings({

    if(is.na(palette)) {

      if(is.na(colours)) colours <- names(MOSL_colours())

      if(reverse) colours <- rev(colours)

      scale_fill_manual(values = unname(MOSL_colours(colours)))


  } else{

      pal <- MOSLR::MOSL_palettes(palette = palette, reverse = reverse)

  if (discrete) {

    discrete_scale("fill", paste0("MOSL", palette), palette = pal, ...)

  } else {

    scale_fill_gradientn(colours = pal(256), ...)

  }
      }
    })
}