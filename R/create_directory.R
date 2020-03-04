#' Create Directory
#'
#' This function creates the required directory for
#' running the MOSLR performance package.
#'
#' @param my.dir character
#' @param projectname character
#'
#' @return
#' @export
#'
#' @examples

create_directory <- function(
  my.dir = choose.dir(),
  projectname = "Project"

) {


# Checking if folder structure exists -------------------------------------

  if(!dir.exists(paste0(my.dir,"/Data"))){
    dir.create(paste0(my.dir,"/Data"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/inputs"))){
    dir.create(paste0(my.dir,"/Data/inputs"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/outputs"))){
    dir.create(paste0(my.dir,"/Data/outputs"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/rdata"))){
    dir.create(paste0(my.dir,"/Data/rdata"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/tracking"))){
    dir.create(paste0(my.dir,"/Data/tracking"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/tracking/mps"))){
    dir.create(paste0(my.dir,"/Data/tracking/mps"))
  }

  if(!dir.exists(paste0(my.dir,"/Data/tracking/ops"))){
    dir.create(paste0(my.dir,"/Data/tracking/ops"))
  }

  if(!dir.exists(paste0(my.dir,"/MonthlyPerfReport"))){
    dir.create(paste0(my.dir,"/MonthlyPerfReport"))
  }

  if(!dir.exists(paste0(my.dir,"/PfmReports"))){
    dir.create(paste0(my.dir,"/PfmReports"))
  }

  path <- file.path(my.dir, paste0(projectname, ".Rproj"))

  template_path <- system.file("templates/template.Rproj",
                               package = "usethis")

  file.copy(template_path, path)

}


