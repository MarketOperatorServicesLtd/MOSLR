#' Data Assurance Process
#'
#' GET TEMPLATE PATHS FROM ONEDRIVE
#' Get possible synced OneDrive locations for templates in Data Assurance folder
#' Check folder exists, if not show warning and skip
#' Check if relevant files in folder, if not show message and skip
#' Check if CSV
#'
#' UPLOAD TEMPLATES TO BLOB NEW
#' Use config file to store container endpoint and SAS key
#' If relevant files present:
#' Clean columns to upper_camel
#' Upload to blob container under New, saving file as "ORG-ID_FileCreatedDate_OriginalFileName"
#'
#' VERIFY TEMPLATES ARE VALID AND VALIDATE COLUMNS
#' Check columns in file match expected columns for either Premises or Meters
#' Verify columns are valid:
#' Measure
#' Action
#' Explanation
#' Create Status columns based on validations:
#' ValidationStatus: Pass / Fail
#' Reason: NA / Validation failure reasons
#'
#' UPLOAD PROCESSED FILES TO BLOB PROCESSED OR REJECTED
#' Include relevant Blob metadata: ORG-ID, FileCreatedDate, OriginalFileName
#' Copy file to OneDrive
#' Save file as "ORG-ID_FileCreatedDate_OriginalFileName_MOSL-Reviewed_ReviewDate.csv"
#' For valid rows:
#' Copy to Blob Processed
#' Save file as "ORG-ID_FileCreatedDate_OriginalFileName.csv"
#' For invalid rows:
#' Copy to Blob Rejected
#' Save file as "ORG-ID_FileCreatedDate_OriginalFileName.csv"
#'
#' @param onedrive.folder Folder or directory within Sharepoint Org site that should contain the new templates to be processed
#' @param conf.loc The path to a configuration file containing Azure storage connection information and keys
#' @param upload.blob Logical specifying whether to upload the new, processed and rejected templates to blob storage
#' @param clean.sharepoint Logical specifying whether to clean-up Sharepoint by moving the processed file
#' @param upload.log Logical specifying whether to upload the log file to blob storage
#' @param ... Optional arguments for underlying functions
#'
#' @return
#' @export
#'
#' @examples

data_assurance_process <- function(
    onedrive.folder = "Data Assurance/New",
    conf.loc = NULL,
    upload.blob = TRUE,
    clean.sharepoint = TRUE,
    upload.log = TRUE,
    ...
    ) {


  # Get template paths from OneDrive

  template_folders <- get_template_paths_from_onedrive(
    root.folder = "Market Operator Services Limited",
    onedrive.folder = onedrive.folder
    )


  # Check for relevant template files

  template_files <- check_for_templates(template_folders, convert.xlsx = TRUE)


  # Connect to blob storage for copying file using details on config file

    conf <- get_config(conf.loc = conf.loc, config.name = "data-assurance")

    bl_endp_key <- AzureStor::storage_endpoint(
      endpoint = conf$endpoint,
      sas = conf$sas,
      service = conf$service
    )

    cont <- AzureStor::blob_container(
      endpoint = bl_endp_key,
      conf$container
    )


  # Upload raw and processed files to Blob
    ## Upload raw templates to Blob New
    ## Process and validate templates
    ## Upload processed templates to Blob Process
    ## Upload rejected templates to Blob Rejected
    ## Cleanup OneDrive
    ## Return log file

  log_tables <- lapply(
    template_files,
    process_and_upload_templates_to_blob,
    cont = cont,
    clean.sharepoint = clean.sharepoint,
    upload.blob = upload.blob
    )

  log_table <- dplyr::bind_rows(log_tables)

  if (upload.log) {

    AzureStor::storage_write_csv(
      object = log_table,
      container = cont,
      file = paste0(
        "Data Assurance/data/logs/data-assurance-process-log_",
        format(Sys.time(), "%Y-%m-%d %H:%M"), ".csv"
        )
      )

  }

  return(log_table)

}


# Helpers -----------------------------------------------------------------

#' Get OneDrive Path
#'
#' @param manual.select Option for whether to select the folder manually.
#' @param root.folder String specifying the name of the root folder (e.g. company name) for OneDrive.
#'
#' @return
#' @export
#'
#' @examples

get_onedrive_path <- function(manual.select = FALSE, root.folder = "Market Operator Services Limited") {

  if (manual.select) {

    path <- choose.dir(default = getwd(), caption = "Select the root OneDrive folder")

    } else {

    user <- strsplit(getwd(), "/")[[1]][3]

    path <- paste0("C:/Users/", user, "/", root.folder)

    }

  return(path)
}


#' Get Template Paths From OneDrive
#'
#' @param root.folder String specifying the name of the root folder (e.g. company name) for OneDrive.
#' @param onedrive.folder Folder or directory within Sharepoint Org site that should contain the new templates to be processed
#'
#' @return
#'
#' @examples

get_template_paths_from_onedrive <- function(root.folder, onedrive.folder) {

  # Check which OneDrive directories in root have a Data Assurance folder

  root_onedrive_path <- get_onedrive_path(manual.select = FALSE, root.folder = root.folder)

  root_onedrive_folders <- list.dirs(root_onedrive_path, recursive = FALSE)

  template_folders <- file.path(root_onedrive_folders, onedrive.folder)

  keep <- dir.exists(file.path(template_folders))

  # Notify user of which directories do not contain a Data Assurance folder

  if (NROW(template_folders[!keep]) > 0) {
    message(paste0("The following directories do not contain the ", onedrive.folder, " folder: "))
    for (folder in template_folders[!keep]) {
      message(folder)
    }
  }

  return(template_folders[keep])

}


#' Check For Templates
#'
#' @param template.folders List of folders that could contain templates
#' @param excluded Pattern for files to be excluded from output
#'
#' @return
#'
#' @examples

check_for_templates <- function(template.folders, excluded = "MOSL-Reviewed", convert.xlsx) {

  folder_files <- list.files(template.folders, full.names = TRUE)

  # Optionally check for excel files and convert to csv

  is_xlsx <- tools::file_ext(folder_files) == "xlsx"

  if (NROW(is_xlsx) > 0 & convert.xlsx) {

    lapply(folder_files[is_xlsx], function(f) {
      df <- readxl::read_excel(f, sheet = 1)
      write.csv(df, gsub("xlsx", "csv", f), row.names = FALSE)
      message(paste(f, "converted to csv file"))

      if (file.exists(gsub("xlsx", "csv", f))) {
        file.remove(f) } else {
          message(paste("Write csv failed for", f))
        }
      }
      )

    folder_files <- list.files(template.folders, full.names = TRUE)
  }

  # Check if CSV or already reviewed by MOSL

  is_csv <- tools::file_ext(folder_files) == "csv"

  mosl_reviewed <- grepl(excluded, folder_files)

  template_files <- folder_files[is_csv & !mosl_reviewed]

  message(paste0(NROW(template_files), " template(s) to be processed."))

  return(template_files)

}


#' Upload File to Blob with Metadata
#'
#' @param processed.template A processed template ready for upload
#' @param cont Azure connection for blob storage
#' @param container.path Container path for blob storage
#' @param org.id Org ID of org site, determined in earlier step
#' @param assurance.type Type of assurance, determined in earlier step
#' @param file.created Time of file creation, determined in earlier step
#' @param template.path Path of the file, determined in earlier step
#'
#' @return
#'
#' @examples

upload_to_blob_with_meta <- function(
  processed.template, cont, container.path, org.id,
  assurance.type, file.created, template.path
  ) {

  AzureStor::storage_write_csv(
    object = processed.template,
    container = cont,
    file = paste0(
      container.path,
      org.id, "_", assurance.type, "_",
      format(Sys.time(), "%Y-%m-%d %H:%M"), ".csv"
    )
  )

  AzureStor::set_storage_metadata(
    cont,
    paste0(
      container.path,
      org.id, "_", assurance.type, "_",
      format(Sys.time(), "%Y-%m-%d %H:%M"), ".csv"
    ),
    ORG_ID = org.id,
    FileCreation = format(file.created, "%Y-%m-%d %H:%M"),
    AssuranceType = assurance.type,
    MOSLReviewDate = format(Sys.Date(), "%Y-%m-%d"),
    FilePath = template.path,
    ProcessRunTime = format(Sys.time(), "%Y-%m-%d %H:%M")
  )

}


#' Upload Assurance Templates to Blob
#'
#' Upload raw templates to Blob New and then processes templates before
#' uploading to Blob processed and OneDrive.
#'
#' @param template.path Path of the file, determined in earlier step
#' @param cont Connection for Azure blob storage
#' @param org.site.regex Regular expression for identifying the name of the org site from the template path
#' @param org.id.regex Regular expression for identifying the org id from the template path
#' @param dowload.template.func User can specify a function for downloading a template from Sharepoint to e.g. change how certain columns are modified
#' @param dowload.template.args Optional arguments for user specified function for downloading template function
#' @param process.template.func User can specify a function for processing the template, but must take only one argument (i.e. template file) and return only a processed template
#' @param clean.sharepoint Logical specifying whether to clean Sharepoint by deleting files etc.
#' @param upload.blob Logical specifying whether to upload files to blob storage
#'
#' @return
#' @export
#'
#' @examples

process_and_upload_templates_to_blob <- function(
  template.path,
  cont,
  org.site.regex = "(-R)|(-W)|(-TEST)",
  org.id.regex = "[A-Z]+(-R|-W|-TEST)",
  dowload.template.func = NULL,
  dowload.template.args = NULL,
  process.template.func = "process_template_default",
  clean.sharepoint,
  upload.blob
  ) {


  # load raw file from OneDrive + clean column names

  org_site <- strsplit(template.path, split = "/")[[1]][grepl(org.site.regex, strsplit(template.path, split = "/"))][5]

  org_id <- unlist(regmatches(org_site, gregexpr(org.id.regex, org_site)))[1]

  if(is.null(dowload.template.func)) {

    template_file <- vroom::vroom(
      template.path,
      progress = FALSE,
      col_types = vroom::cols(),
      .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")
      ) %>%
      dplyr::mutate(
        DateOfReview = as.Date(DateOfReview, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
        InitialReadDate = as.Date(InitialReadDate, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
        )

  } else {

    template_file <- do.call(dowload.template.func, dowload.template.args)

  }


  # upload cleaned template to Blob New

  file_name <- tools::file_path_sans_ext(basename(template.path))
  file_created <- file.info(template.path)$ctime

  if (upload.blob) {

    AzureStor::storage_write_csv(
      object = template_file,
      container = cont,
      file = paste0(
        "Data Assurance/data/New/",
        org_id, "_", format(file_created, "%Y-%m-%d %H:%M"),
        "_", file_name, ".csv"
        )
      )

  }


  # Process/Validate Template

  processed_template <- do.call(process.template.func, list(template_file))


  # Upload Valid Templates and Metadata to Blob Process

  assurance_type <- unique(processed_template$AssuranceType)

  if (upload.blob) {

    if (sum(processed_template$ValidEntry) > 0) {

      upload_to_blob_with_meta(
        cont = cont,
        processed.template = processed_template[processed_template$ValidEntry,],
        container.path = "Data Assurance/data/Processed/",
        org.id = org_id,
        assurance.type = assurance_type,
        file.created = file_created,
        template.path = template.path
      )

    }


    # Upload Invalid templates to Blob Rejected

    if (sum(!processed_template$ValidEntry) > 0) {

      upload_to_blob_with_meta(
        cont = cont,
        processed.template = processed_template[!processed_template$ValidEntry,],
        container.path = "Data Assurance/data/Rejected/",
        org.id = org_id,
        assurance.type = assurance_type,
        file.created = file_created,
        template.path = template.path
      )

    }

  }


  # Upload reviewed templates to OneDrive and cleanup Sharepoint files

  if (clean.sharepoint) {

    write_success <- tryCatch(
      vroom::vroom_write(
        processed_template,
        paste0(
          stringr::str_replace(stringr::str_sub(template.path, end = nchar(template.path) - 4), pattern = "New", "Processed"),
          "_MOSL-Reviewed_",
          format(Sys.Date(), "%Y-%m-%d"),
          ".csv"
        ),
        delim = ",", na = ""
      ),
      error = function(e) {
        message(
          "Write failure: ",
          paste0(
            stringr::str_replace(stringr::str_sub(template.path, end = nchar(template.path) - 4), pattern = "New", "Processed"),
            "_MOSL-Reviewed_",
            format(Sys.Date(), "%Y-%m-%d"),
            ".csv"
            )
        )
      }
    )


    # Cleanup OneDrive files

    if (!any(class(write_success) == "error")) file.remove(template.path) else message(paste0("Not removed: ", template.path))

  } else {

    write_success <- NULL

  }


  # Keep a log of all the files and their information

  log_table <-
    dplyr::bind_rows(
      tibble::tibble(
        ProcessRunTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
        MOSLReviewDate = format(Sys.Date(), "%Y-%m-%d"),
        FilePath = template.path,
        FileCreated = file_created,
        AssuranceType = assurance_type,
        TemplateValidationStatus = dplyr::if_else(
          assurance_type == "InvalidColumns", "FAILED", "PASSED"
          ),
        RowCount = nrow(processed_template),
        ValidEntries = sum(processed_template$ValidEntry),
        ValidEntriesPerc = mean(processed_template$ValidEntry, na.rm = TRUE) * 100,
        WriteSuccess = !any(class(write_success) == "error")
      )
    )

  return(log_table)

}


#' Process Template Default
#'
#' This function is the default function for processing a given template, and outputs
#' a processed file.
#'
#' @param template_file The template file to be processed
#' @param prem.col Optional list of columns detailing expected to be in a premises template
#' @param meter.col Optional list of columns detailing expected to be in a meter template
#'
#' @return
#' @export
#'
#' @examples

process_template_default <- function(template_file, prem.col = NULL, meter.col = NULL) {

  # Sets the expected measures and actions for validating templates

  measures <- tibble::tibble(
    Type = "Premises",
    Measure = c("UPRN", "VOA", "LTV", "VAC")
  ) %>%
    dplyr::bind_rows(
      tibble::tibble(
        Type = "Meters",
        Measure = "GIS"
      )
    )

  actions <- tibble::tibble(
    Type = "Premises",
    Action = c("V", "R", "Verified", "Removed", "A", "Assured", "A/ Assured")
  ) %>%
    dplyr::bind_rows(
      tibble::tibble(
        Type = "Meters",
        Action = c("V", "R", "Verified", "Removed", "A", "Assured", "A/ Assured")
      )
    )

  reviews <- tibble::tibble(
    Type = "Premises",
    Review = c(
      "Desktop Review",
      "Phonecall to or From Site",
      "Site Visit",
      "Other",
      "Other (Comment)"
    )
  )

  explanations <- tibble::tibble(
    Type = "Premises",
    Explanation = c(
      "Verified Vacant Leak on Coms Pipe",
      "Verified Vacant Leak on Supply",
      "Verified Vacant Security Guard",
      "Verified Vacant TBS",
      "Verified Vacant Permanent Disconnection Request",
      "Verified Vacant Temporary Disconnection Request",
      "Verified Vacant HH Deregistration",
      "Verified Vacant HH Other (Comment)",
      "Verified Vacant HH Other"
    )
  )

  issues <- tibble::tibble(
    Type = "Meters",
    Issue = c(
      "UPRN / POSTCODE CENTRE",
      "FAR UPRN / POSTCODE CENTRE",
      ">20 METERS STACKED"
    )
  )


  # Sets the default expected columns from templates if not specified in the function call.

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

  # Creates blank df of colnames to enable checking type of template

  cols <- unique(c(prem.col, meter.col))

  cols_df <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = ~ cols) %>%
    dplyr::mutate_at(dplyr::vars(DateOfReview, InitialReadDate), as.Date) %>%
    dplyr::mutate_at(dplyr::vars(
      Spidcore, Measure, Action, Issue, TypeOfReviewDropdown,
      Explanation, Spid, MeterManufacturer, MeterSerialNo), as.character
    )

  # Processsing template file using col names and set of measure and metrics

  processed_template <- template_file %>%
    dplyr::bind_rows(cols_df) %>%
    dplyr::mutate(
      AssuranceType = dplyr::case_when(
        all(colnames(template_file) %in% prem.col) ~ "Premises",
        all(colnames(template_file) %in% meter.col) ~ "Meters",
        TRUE ~ "InvalidColumns"
      ),
      ValidMeasure = dplyr::case_when(
        AssuranceType == "Premises" & Measure %in% measures$Measure[measures$Type=="Premises"] ~ TRUE,
        AssuranceType == "Meters" ~ TRUE,
        TRUE ~ FALSE
      ),
      ValidAction = dplyr::case_when(
        AssuranceType == "Premises" & Action %in% actions$Action[actions$Type=="Premises"] ~ TRUE,
        AssuranceType == "Meters" & Action %in% actions$Action[actions$Type=="Meters"] ~ TRUE,
        TRUE ~ FALSE
      ),
      ValidReview = dplyr::case_when(
        AssuranceType == "Premises" & TypeOfReviewDropdown %in% reviews$Review[reviews$Type=="Premises"] ~ TRUE,
        AssuranceType == "Meters" ~ TRUE,
        TRUE ~ FALSE
      ),
      ValidExplanation = dplyr::case_when(
        AssuranceType == "Premises" & Explanation %in% explanations$Explanation[explanations$Type=="Premises"] ~ TRUE,
        AssuranceType == "Meters" ~ TRUE,
        TRUE ~ FALSE
      ),
      ValidIssue = dplyr::case_when(
        AssuranceType == "Premises" ~ TRUE,
        AssuranceType == "Meters" & Issue %in% issues$Issue[issues$Type=="Meters"] ~ TRUE,
        TRUE ~ FALSE
      ),
      ValidEntry = dplyr::if_else(ValidMeasure & ValidAction & ValidReview & ValidExplanation & ValidIssue, TRUE, FALSE)
    ) %>%
    tidyr::drop_na(intersect(prem.col, meter.col))

}
