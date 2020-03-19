#' Create Directory
#'
#' This function creates the required directory for
#' running the MOSLR performance package.
#'
#' @param my.dir character
#' @param foldername character
#' @param projectname character
#'
#' @return
#' @export
#'
#' @examples

create_directory <- function(
  my.dir = utils::choose.dir(),
  foldername = "PerformanceMonitoring",
  projectname = "Project"

) {


# List of folders to be checked and created in the directory --------------

  folder_list <- c(
    paste0(my.dir, "/", foldername),
    paste0(my.dir, "/", foldername, "/data"),
    paste0(my.dir, "/", foldername, "/data/inputs"),
    paste0(my.dir, "/", foldername, "/data/outputs"),
    paste0(my.dir, "/", foldername, "/data/rdata"),
    paste0(my.dir, "/", foldername, "/data/tracking"),
    paste0(my.dir, "/", foldername, "/data/tracking/mps"),
    paste0(my.dir, "/", foldername, "/data/tracking/ops"),
    paste0(my.dir, "/", foldername, "/MonthlyPerfReport"),
    paste0(my.dir, "/", foldername, "/PfmReports")
    )


# Checking and creating folder structure ----------------------------------

  for(folder in folder_list){
    if(!dir.exists(folder)) {
      dir.create(folder)
    }
    }


# Creating a project in the directory, if necessary -----------------------

  path <- file.path(my.dir, paste0(foldername, "/", projectname, ".Rproj"))

  template_path <- system.file("templates/template.Rproj", package = "usethis")

  if(!file.exists(path)){
    file.copy(template_path, path)
  }
  }
