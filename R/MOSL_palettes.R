

#' MOSL_palettes
#'
#' A function containing three palettes with either MOSL's core colours, secondary colours or all colours.
#'
#' @param palette
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
#' Available palettes are:\cr
#' 1) 'core', (includes colours 'grey', 'light blue' and 'dark blue')\cr
#' 2) 'secondary' (includes colours 'green', 'orange', 'yellow' and 'purple') \cr
#' 3) 'all' (includes all 7 previously mentioned colours)
#'
#'


MOSL_palettes <- function(
    palette = "all",
    reverse = FALSE,
    ...
    ) {

  MOSL_pal <- list(
    `core` = MOSLR::MOSL_colours("grey", "light blue", "dark blue"),

    `secondary` = MOSLR::MOSL_colours("green", "orange", "yellow", "purple"),

    `all` = MOSL_colours()
  )

  pal <- MOSL_pal[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

