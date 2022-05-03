
#' scale_colour_MOSL
#'
#' This function offers a MOSL colour scale for ggplot2
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
#' See \code{\link{MOSL_palettes}} for all available colour palettes
#' @param colours
#' Specifies the MOSL colours when palette is NULL. When not specified, all colours are selected. See \code{\link{MOSL_colours}} for all available colours
#' @param reverse
#' Boolean variable to indicate whether to reverse the order of the scale or not
#' @param discrete
#' Boolean variable to indicate whether to use a discrete scale or not


scale_colour_MOSL <- function(colours = NULL, reverse = FALSE, palette = NULL, discrete = TRUE, ...) {
  suppressWarnings({

    if(is.null(palette)) {

      if(is.null(colours)) colours <- names(MOSL_colours())

      if(reverse) colours <- rev(colours)

      ggplot2::scale_colour_manual(values = unname(MOSL_colours(colours)))

    } else {

      pal <- MOSLR::MOSL_palettes(palette = palette, reverse = reverse)

      if (discrete) {

        ggplot2::discrete_scale("colour", paste0("MOSL", palette), palette = pal, ...)

      } else {

        ggplot2::scale_colour_gradientn(colours = pal(256), ...)

      }
    }
  })
}

