#' Get Config File
#'
#' This function wraps around config::get and adds additional logic to
#' locate and return a relevant config file and sets the needed
#' environment variables.
#'
#' @param conf.loc Path to config file that can be specified upfront.
#' @param config.name Name of the section of the config file that will be used.
#'
#' @return
#' @export
#'
#' @examples

get_config <- function(conf.loc = NULL, config.name = "default") {

  Sys.setenv(R_CONFIG_ACTIVE = config.name)

  if (is.null(conf.loc)) {

    err <- try(conf <- config::get(), silent = TRUE)

    if ("try-error" %in% class(err)) {

      conf <- config::get(file = choose.files(caption = "Select configuration file"))

    }

  } else if (conf.loc == "select") {

    conf <- config::get(file = choose.files(caption = "Select configuration file"))

  } else {

    conf <- config::get(file = conf.loc, config = config.name)
  }

  return(conf)

}
