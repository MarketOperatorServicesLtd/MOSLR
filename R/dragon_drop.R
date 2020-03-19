#' Drag and drop files from one location to another based on folder name to a specified directory
#'
#' @param destination character
#' @param origin character
#' @param folder.name character
#' @param folder.ext character
#' @param sub.folder character
#' @param file.type factor
#' @param folder.list factor
#' @param new.folder character
#' @param distribute logical
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples

dragon_drop <- function(
  origin =
    utils::choose.dir(caption = "Select the origin directory that folders or file(s) should be copied from."),
  destination =
    utils::choose.dir(caption = "Select the destination directory that folders or file(s) should be copied to."),
  folder.name = format(Sys.Date(), format = "%Y-%m"),
  folder.ext = NULL,
  sub.folder = NULL,
  file.type = NULL,
  folder.list = "destination",
  new.folder = TRUE,
  distribute = FALSE
  ){

  folder.list <- if (folder.list == "origin") {

    dir(origin)

  } else if (folder.list == "destination") {

    if (is.null(folder.ext)) {

      dir(destination)

    } else {

      gsub(
        folder.ext, "",
        dir(destination)[grep(paste0("*.", folder.ext, "$"), dir(destination))]
        )
    }

  } else if (is.vector(folder.list) & is.character(folder.list)) {

    folder.list

  } else {

    stop("error: specify folder.list as either \"from\" or \"to\" or a valid character vector of relevant folder names")

  }

  for (i in folder.list) {

    if (new.folder) {
      dir.create(
        file.path(
          destination,
          paste0(i, ifelse (is.null(folder.ext), "", folder.ext)),
          ifelse (is.null(sub.folder), "", sub.folder),
          folder.name
          )
        )
    }

    file.copy(
      from =
        list.files(
          file.path(origin, ifelse (distribute, "", i)),
          pattern = file.type,
          full.names = TRUE
          ),
      to =
        file.path(
          destination,
          paste0(i, folder.ext),
          ifelse (is.null(sub.folder), "", sub.folder),
          folder.name
          )
      )

  }

}
