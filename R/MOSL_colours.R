
#' MOSL_colours
#'
#' A function containing all of MOSL brand colours
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' @details
#' The available colours are: 'grey', 'light blue', 'dark blue', green', 'orange', 'yellow' and 'purple'
#'


MOSL_colours <- function(...){
  colours <- c(
    `grey` = "#425563",
    `light blue` = "#05C3DE",
    `dark blue` = "#005F83",
    `green` = "#00A499",
    `orange` = "#FFAA4D",
    `yellow` = "#F9E547",
    `purple` = "#8866BC")

  cols <- c(...)

  if (is.null(cols))
    return (colours)

  colours[cols]
}

