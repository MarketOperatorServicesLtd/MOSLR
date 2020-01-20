dragon_drop <- function(dir.to = getwd(), dir.from = choose.dir(caption = "Select the directory that folders  should be copied from."), folder.name = format(Sys.Date(), format = "%Y-%m"), folder.ext = NULL, sub.folder = NULL, file.type = NULL, tp.list = "to", new.folder = TRUE) {

  tp.list <- if (tp.list == "from") {
    dir(dir.from)
  } else if (tp.list == "to") {
    if (is.null(folder.ext)) {
      dir(dir.to)
    } else {
      gsub(folder.ext, "", dir(dir.to)[grep(paste0("*.", folder.ext, "$"), dir(dir.to))])
    }
  } else if (is.vector(tp.list) & is.character(tp.list)) {
    tp.list
  } else {
    stop("error: specify tp.list as either \"from\" or \"to\" or a valid character vector of relevant ORG IDs")
  }

  for (i in tp.list) {

    if (new.folder) {
      dir.create(file.path(dir.to, paste0(i, ifelse (is.null(folder.ext), "", folder.ext)), ifelse (is.null(sub.folder), "", sub.folder), folder.name))
    }

    file.copy(
      from = list.files(file.path(dir.from, i), pattern = file.type, full.names = TRUE),
      to = file.path(dir.to, paste0(i, folder.ext), ifelse (is.null(sub.folder), "", sub.folder), folder.name)
    )

  }

}
