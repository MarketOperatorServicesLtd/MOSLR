
#' Assurance process data move
#'
#' This function moves data from Trading Party Sharepoint sites
#' to a MOSL blob storage. It also tracks all of the changes and
#' validates the received data. Validated files are then sent back
#' to the Trading Party's Sharepoint site
#'
#' @param period.from A date. Files that were created after this date will be processed. Defaults to the date exactly a month before the current date.
#' @param period.to A date. Files that were created before this date will be processed. Defaults to the next day from the current date, to pick up all of the files created on the current day.
#' @param onedrive.path Path to where all of the OneDrive folders are synced. If NULL, function attempts to guess this.
#' @param conf.loc Location of the configuration file that keeps the Blob storage access keys. If NULL, a pop-up window appears, which can be used to locate the file
#' @param tp_sites A list of all the Trading Party sites from where to look for files. By default all of the Trading Party sites in the Onedrive folder will be used
#' @param folder Folder in the Trading Party site, where the files are kept
#' @param combine A boolean, indicating whether the raw files should be combined into one file
#' @param prem.col The column names of the Premises file template
#' @param meter.col The column names of the meter file template
#'
#' @return Returns a table which contains information about all of the files processed
#'
#' @export
#' @importFrom lubridate %m-%
#' @importFrom lubridate %m+%
#' @examples

assurance_process_data_move <- function(
    period.from = NULL,
    period.to = NULL,
    onedrive.path = NULL,
    conf.loc = NULL,
    tp_sites = NULL,
    folder = "Data Assurance",
    combine = FALSE,
    prem.col = NULL,
    meter.col = NULL
    ) {

  if (is.null(period.from))
    period.from <- Sys.Date() %m-% months(1)
  if (is.null(period.to))
    period.to <- Sys.Date() %m+% days(1)
  if (is.null(prem.col))
    prem.col <-
      c(
        "DateOfReview",
        "Spidcore",
        "Measure",
        "Action",
        "Issue",
        "TypeOfReviewDropdown",
        "Explanation"
      )
  if (is.null(meter.col))
    meter.col <-
      c(
        "DateOfReview",
        "Spid",
        "MeterManufacturer",
        "MeterSerialNo",
        "InitialReadDate",
        "Action",
        "Issue",
        "Explanation"
      )


  period.to <- as.Date(period.to)

  period.from <- as.Date(period.from)

  # Getting the configuration file

  Sys.setenv(R_CONFIG_ACTIVE = "data-assurance")

  if (is.null(conf.loc)) {
    err <- try(conf <- config::get(), TRUE)
    if ("try-error" %in% class(err))
      conf <-
        config::get(file = choose.files(caption = "Select configuration file"))
  } else if (conf.loc == "select") {
    conf <-
      config::get(file = choose.files(caption = "Select configuration file"))
  } else {
    conf <- config::get(file = conf.loc, config = "data-assurance")
  }


  # Connecting to the blob storage

  bl_endp_key <- AzureStor::storage_endpoint(
    endpoint = conf$endpoint,
    sas = conf$sas
    )

  cont <- AzureStor::blob_container(
    bl_endp_key,
    "data-assurance-container"
    )

  # If the path to onedrive isn't given, attempt to guess it

  if (is.null(onedrive.path)) {
    user <- strsplit(getwd(), "/")[[1]][3]

    onedrive.path <- paste0("C:/Users/", user, "/Market Operator Services Limited")
  }

  # If the list of all TP sites is unspecified, get a list of all the TP sites synced

  if (is.null(tp_sites)) {
    tp_sites <- list.dirs(onedrive.path, recursive = FALSE)

    tp_sites <- tp_sites[grepl("(-R)|(-W)", tp_sites)]
    tp_sites <- paste0(tp_sites, "/", folder)
  }

  # Creating empty tables for combined data

  if (combine) {
    premises_table <- tibble::tibble()
    meter_table <- tibble::tibble()
  }

  log_table <- tibble::tibble()

  for (tp_site in tp_sites) {
    # If the Data Assurance folder doesn't exist, move to the next Trading Party
    if (!dir.exists(tp_site)) {
      cat(crayon::red(paste("ERROR:"), tp_site, "doesn't exist\n"))
    } else {

      if(grepl("-R", tp_site)){
        tp_type <- "-R"
      } else{
        tp_type <- "-W"
      }

      tp <-
        strsplit(tp_site, split = "/")[[1]][grepl("(-R)|(-W)", strsplit(tp_site, split = "/")[[1]])]
      tp_clean <-
        substr(tp, 1, unlist(gregexpr(tp_type, tp))[1] + 1)

      # Get a list of all the files in the Data Assurance folder

      all_files <- list.files(tp_site, full.names = TRUE)

      iscsv <- tools::file_ext(all_files) == "csv"
      isnew <-
        file.info(all_files, extra_cols = FALSE)$ctime >= period.from &
        file.info(all_files, extra_cols = FALSE)$ctime <= period.to
      ismosl <- grepl("MOSL_Reviewed.csv", all_files)

      # Filter for only the files that are csv files, new and aren't MOSL's responses

      latest_files <- all_files[isnew & iscsv & !ismosl]

      # If there's at least one new file do the following:

      if (length(latest_files) > 0) {
        for (FilePath in latest_files) {
          tryCatch(
            expr = {
              # Read the csv file into R and clean the column names
              File <- vroom::vroom(
                FilePath,
                progress = FALSE,
                col_types = vroom::cols(),
                .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")
              )


              Filename <- strsplit(FilePath, split = "/")
              Filename <- Filename[[1]][length(Filename[[1]])]
              FileCreated <- file.info(FilePath)$ctime
              AzureStor::storage_write_csv(
                File,
                cont,
                paste0(
                  "Data Assurance/data/New/",
                  tp_clean,
                  "_",
                  format(FileCreated, "%Y-%m-%d %H:%M"),
                  "_",
                  Filename
                )
              )

              # If all the column names match with the Premises template, consider it to be the Premises file

              if (all(prem.col == colnames(File))) {
                assurance_type <- "Premises"
                # Change the DateOfReview column into a Date type

                if (class(File$DateOfReview) == "character")
                  File$DateOfReview <-
                    as.Date(File$DateOfReview,
                            tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))

                # Check whether each of the rows in the file is valid and matches the template

                File <- File %>%
                  dplyr::mutate(
                    ValidMeasure = dplyr::if_else(Measure %in% c("UPRN", "VOA", "LTV", "VAC"), TRUE, FALSE),
                    ValidAction = dplyr::if_else(
                      Action %in% c("V", "R", "Verified", "Removed"),
                      TRUE,
                      FALSE
                    ),
                    ValidReview = dplyr::if_else(
                      TypeOfReviewDropdown %in% c(
                        "Desktop Review",
                        "Phonecall to or From Site",
                        "Site Visit",
                        "Other",
                        "Other (Comment)"
                      ),
                      TRUE,
                      FALSE
                    ),
                    ValidExplanation = dplyr::if_else(
                      Explanation %in% c(
                        "Verified Vacant Leak on Coms Pipe",
                        "Verified Vacant Leak on Supply",
                        "Verified Vacant Security Guard",
                        "Verified Vacant TBS",
                        "Verified Vacant Permanent Disconnection Request",
                        "Verified Vacant Temporary Disconnection Request",
                        "Verified Vacant HH Deregistration",
                        "Verified Vacant HH Other (Comment)",
                        "Verified Vacant HH Other"
                      ),
                      TRUE,
                      FALSE
                    ),
                    ValidEntry = dplyr::if_else(
                      ValidMeasure &
                        ValidAction & ValidReview & ValidExplanation,
                      TRUE,
                      FALSE
                    )
                  )

                # If all the column names match with the meter template, consider it to be the meter file
              } else if (all(meter.col == colnames(File))) {
                assurance_type <- "Meters"
                if (class(File$DateOfReview) == "character")
                  File$DateOfReview <-
                    as.Date(File$DateOfReview,
                            tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))

                # Check whether each of the rows in the file is valid and matches the meter template

                File <- File %>%
                  dplyr::mutate(
                    # ValidMeasure = dplyr::if_else(Measure %in% c("GIS"), TRUE, FALSE),
                    ValidAction = dplyr::if_else(
                      Action %in% c("V", "R", "Verified", "Removed"),
                      TRUE,
                      FALSE
                    ),
                    ValidIssue = dplyr::if_else(
                      Issue %in% c(
                        "UPRN / POSTCODE CENTRE",
                        "FAR UPRN / POSTCODE CENTRE",
                        ">20 METERS STACKED"
                      ),
                      TRUE,
                      FALSE
                    ),
                    ValidExplanation = dplyr::if_else(is.na(Explanation), FALSE, TRUE),
                    ValidEntry = dplyr::if_else(ValidAction &
                                                  ValidIssue & ValidExplanation, TRUE, FALSE)
                  )
              } else {
                assurance_type <- "Unknown"
              }

              # Save some information about the current file: filename, when that file was created, the type of assurance, whether it failed validation or was uploaded,
              # how many rows in that file and the percentage that passed validation

              entry <- tibble::tibble(
                FileName = FilePath,
                FileCreated = file.info(FilePath)$ctime,
                assurance_type = assurance_type,
                Action = ifelse(
                  assurance_type == "Unknown",
                  "Failed Validation",
                  "Uploaded"
                ),
                NrRows = nrow(File),
                PropValidEntries = scales::percent(
                  dplyr::if_else(assurance_type == "Unknown", 0, mean(File$ValidEntry)),
                  accuracy = 0.1
                )
              )

              # Keep a log of all the files and their information

              log_table <- dplyr::bind_rows(log_table, entry)

              # If we want to combine all the files into one, we do that here

              if (combine) {
                if (assurance_type == "Meters") {
                  File$FileCreated <- entry$FileCreated
                  meter_table <- dplyr::bind_rows(meter_table, File)
                } else if (assurance_type == "Premises") {
                  File$FileCreated <- entry$FileCreated
                  premises_table <-
                    dplyr::bind_rows(premises_table, File)
                }
                # Otherwise save the file in a Blob storage and also put a copy of the file with added information on the Trading Party Sharepoint site
              } else {
                if (assurance_type == "Meters" | assurance_type == "Premises") {
                  File$FileCreated <- as.Date(entry$FileCreated)
                  File$MOSLReviewDate <-
                    format(Sys.Date(), "%Y-%m-%d")
                  AzureStor::storage_write_csv(
                    File,
                    cont,
                    paste0(
                      "Data Assurance/data/Processed/",
                      tp_clean,
                      "_",
                      assurance_type,
                      "_",
                      format(entry$FileCreated, "%Y-%m-%d %H:%M"),
                      ".csv"
                    )
                  )
                  vroom::vroom_write(
                    File,
                    paste0(
                      stringr::str_sub(FilePath, end = nchar(FilePath) - 4),
                      "_MOSL_Reviewed.csv"
                    ),
                    delim = ",",
                    na = ""
                  )
                } else{
                  AzureStor::storage_write_csv(
                    File,
                    cont,
                    paste0(
                      "Data Assurance/data/Rejected/",
                      tp_clean,
                      "_Rejected_",
                      format(entry$FileCreated, "%Y-%m-%d %H:%M"),
                      ".csv"
                    )
                  )
                }
              }
            },

            error = function(e) {
              print(e)
            }
          )
        }
        cat(crayon::green(paste("Files processed for:", tp_site, "\n")))
      } else {
        cat(crayon::yellow(paste("No new csv files for", tp_site, "\n")))
      }
    }
  }

  # If we combined any of the tables in the previous steps, we upload them to the Blob storage here

  if (combine) {
    if (nrow(meter_table) > 0) {
      AzureStor::storage_write_csv(meter_table,
                                   cont,
                                   paste0(
                                     "Data Assurance/data/meter_Data_",
                                     format(Sys.Date(), "%Y-%m-%d"),
                                     ".csv"
                                   ))
    }
    if (nrow(premises_table) > 0) {
      AzureStor::storage_write_csv(
        premises_table,
        cont,
        paste0(
          "Data Assurance/data/Premises_Data_",
          format(Sys.Date(), "%Y-%m-%d"),
          ".csv"
        )
      )
    }
  }


  return(log_table)
}
