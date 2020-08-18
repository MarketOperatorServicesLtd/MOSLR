

#' scale_fill_MOSL
#'
#' This function offers a MOSL fill scale for ggplot2
#'
#' @param palette
#' @param discrete
#' @param reverse
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


scale_fill_MOSL <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MOSLR::MOSL_palettes(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("MOSL", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

