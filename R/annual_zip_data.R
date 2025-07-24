

#' Get the End-of-Year ZIP File for a Given Year
#'
#' Searches for an end-of-year BRFSS data ZIP file in a given folder.
#'
#' @param year Integer. The full 4-digit or 2-digit year to search for (e.g., 2023 or 23).
#' @param folder Character. The directory to search in. Defaults to `get_downloads_folder()`.
#'
#' @return A character vector of matching file paths (could be length 0 if none found).
#'
#' @export
eoy_data_file <- function(year, folder = get_downloads_folder()) {

  yr <- year%%100
  file <- list.files(folder,
                     pattern = paste0("^VI", yr , ".*END_OF_YEAR.*[.]zip"),
                     full.names = T, ignore.case = T)

  file
}



#' Unzip BRFSS Data File by Extension
#'
#' Finds and unzips a specific data file type (e.g., "XPT", "DTA") from ZIP archives in a directory.
#'
#' @param ext Character. File extension (without dot) to identify the desired data file (e.g., `"XPT"`).
#' @param data_dir Character. Directory where zipped files are located and unzipped.
#'
#' @return Invisibly returns a list of extracted file paths. Removes ZIP files after extraction.
#'
#' @importFrom purrr map_chr map
#' @importFrom stringr str_trim
#' @export
unzip_data_file <- function(ext, data_dir) {

  ext_file_zip <- list.files(data_dir, pattern = paste0(".*", ext, ".zip$"),
                             ignore.case = T, full.names = T)

  ext_file <- purrr::map_chr(ext_file_zip, ~unzip(zipfile = .x, list = T) %>%
                               pull(Name) %>%
                               {stringr::str_trim(.[1])})

  x <- purrr::map(ext_file_zip, ~unzip(zipfile = .x, exdir = data_dir))
  file.remove(ext_file_zip)
}

#' Process a BRFSS Annual ZIP Archive
#'
#' Unzips the main archive and its nested data/report ZIPs into appropriate subfolders.
#'
#' @param zip_file Character. Path to the annual BRFSS ZIP file.
#' @param dir Character. Working directory (defaults to `tempdir()`). Subdirectories are created within it.
#'
#' @return Invisibly returns `NULL`. Side effects: unzips and organizes data files.
#'
#' @importFrom dplyr filter pull
#' @export
process_annual_zipfile <- function(zip_file, dir = tempdir()) {

  #  create necessary dirs

  data_dir <- paste0(dir, "/brfss_data/")
  report_dir <- paste0(dir, "/brfss_reports/")

  dir.create(path = dir, showWarnings = F)
  dir.create(path = report_dir, showWarnings = F)
  dir.create(path = data_dir, showWarnings = F)

  #  get the list (data frame) of filenames  (usually two files, data and reports)
  df_zip_files <- unzip(zipfile = zip_file, list = T)

  #   get the data zip file name (not the reports zip file)
  data_zip <- df_zip_files %>%
    filter(grepl("_DATA[.]zip",Name, ignore.case = T)) %>%
    pull(Name)

  report_zip <- df_zip_files %>%
    filter(grepl("_REPORTS[.]zip",Name, ignore.case = T)) %>%
    pull(Name)

  #   unzip this zip file into the tempdir
  unzip(zipfile = zip_file, files = data_zip, exdir = data_dir)
  unzip(zipfile = zip_file, files = report_zip, exdir = report_dir)

  # get the name of the data zip and unzip it

  data_zip_file <- list.files(data_dir, pattern = "data[.]zip$", ignore.case = T, full.names = T)
  unzip(zipfile = data_zip_file, exdir = data_dir)

  #       at this point all of the data files (in zip format) are in the data folder
  # ========================================================================================
  file.remove(data_zip_file)


  unzip_data_file("XPT", data_dir = data_dir)
  unzip_data_file("ASCII", data_dir = data_dir)
  unzip_data_file("DTA", data_dir = data_dir)
  unzip_data_file("SAV", data_dir = data_dir)
  unzip_data_file("INC", data_dir = data_dir)

  # get the name of the report zip and unzip it

  report_zip_file <- list.files(report_dir, pattern = "reports[.]zip$", ignore.case = T,
                                full.names = T)

  unzip(zipfile = report_zip_file, exdir = report_dir)

  #       at this point all of the data files (in zip format) are in the data folder
  # ========================================================================================
  file.remove(report_zip_file)

}

#' Move BRFSS Data Files into Structured Folders
#'
#' Moves unzipped BRFSS ASCII and SAS files into appropriate subdirectories (`ascii/` and `sas/`) under a specified folder.
#'
#' @param from Character. Source directory where `brfss_data/` is located (default is `tempdir()`).
#' @param to Character. Destination root folder. If `NULL`, it's resolved from a BRFSS pattern config.
#'
#' @return Invisibly returns `NULL`. Side effect: moves files to new locations.
#'
#' @importFrom brfss my.brfss.patterns
#' @export
move_annual_data_files <- function(from = tempdir(), to = NULL) {

  params <-brfss::my.brfss.patterns()

  if(is.null(to)) to <- apply.pattern(name = "brfss_annual_raw_data_folder" , params)

  from <- paste0(normalizePath(from, winslash = "/"),"/brfss_data")

  data_files <- list.files(from, full.names = T)

  asc_file <- which(grepl("COMP.DAT", data_files))
  ascii_data_file <- data_files[asc_file]

  sas_data_files <- data_files[-asc_file]

  raw_dir <- to
  ascii_dir <- paste0(raw_dir, "ascii")
  sas_dir <- paste0(raw_dir, "sas")

  #  dir.create(raw_dir, recursive = T)
  dir.create(ascii_dir, recursive = T, showWarnings = F)
  dir.create(sas_dir, recursive = T, showWarnings = F)

  file.copy(from = ascii_data_file, to = ascii_dir)
  file.copy(from = sas_data_files, to = sas_dir)

}


#' Move BRFSS Report Files into Structured Folders
#'
#' Moves unzipped BRFSS ASCII and SAS files into appropriate subdirectories (`codebook`) under a specified folder.
#'
#' @param from Character. Source directory where `brfss_reports/` is located (default is `tempdir()`).
#' @param to Character. Destination root folder. If `NULL`, it's resolved from a BRFSS pattern config.
#'
#' @return Invisibly returns `NULL`. Side effect: moves files to new locations.
#'
#' @importFrom brfss my.brfss.patterns
#' @export
move_annual_report_files <- function(from = tempdir(), to = NULL) {

  params <-brfss::my.brfss.patterns()

  if(is.null(to)) to <- apply.pattern(name = "brfss_annual_raw_data_folder" , params)

  from <- paste0(normalizePath(from, winslash = "/"),"/brfss_reports")

  report_files <- list.files(from, full.names = T)

  codebook_file <- report_files[which(grepl("CODE_LLCP.RTF", report_files))]

  raw_dir <- to
  codebook_dir <- paste0(raw_dir, "codebook")

  #  dir.create(raw_dir, recursive = T)
  dir.create(codebook_dir, recursive = T, showWarnings = F)

  file.copy(from = codebook_file, to = codebook_dir)

}

#' Process a SASOUT ZIP Archive
#'
#' Unzips the SASOUT archive ZIP into the appropriate subfolder.
#'
#' @param zip_file Character. Path to the annual SASOUT ZIP file.
#' @param dir Character. Working directory (defaults to `tempdir()`). Subdirectories are created within it.
#'
#' @return Invisibly returns `NULL`. Side effects: unzips and organizes data files.
#'
#' @importFrom dplyr filter pull
#' @export
process_sasout_zipfile <- function(zip_file, dir = tempdir()) {

  #  create necessary dirs

  sasout_dir <- paste0(dir, "/metadata/")

  dir.create(path = sasout_dir, showWarnings = F)

  #  get the list (data frame) of filenames  (usually two files, data and reports)
  df_zip_files <- unzip(zipfile = zip_file, list = T)
  sasout_zip <- df_zip_files %>% head(1) %>% pull(Name)
  #   get the data zip file name (not the reports zip file)

  #   unzip this zip file into the tempdir
  unzip(zipfile = zip_file, files = sasout_zip, exdir = sasout_dir)

}

#' Move BRFSS SASOUT File into Structured Folders
#'
#' Moves unzipped BRFSS SASOUT file into appropriate subdirectory (`metadata`) under a specified folder.
#'
#' @param from Character. Source directory where `metadata/` is located (default is `tempdir()`).
#' @param to Character. Destination root folder. If `NULL`, it's resolved from a BRFSS pattern config.
#'
#' @return Invisibly returns `NULL`. Side effect: moves files to new locations.
#'
#' @importFrom brfss my.brfss.patterns
#' @export
move_metadata_files <- function(from = tempdir(), to = NULL) {

  params <-brfss::my.brfss.patterns()

  if(is.null(to)) to <- brfss::apply.pattern(name = "brfss_annual_raw_metadata_folder" , params)

  from <- paste0(normalizePath(from, winslash = "/"),"/metadata")

  metadata_files <- list.files(from, full.names = T)

  raw_dir <- to

  #  dir.create(raw_dir, recursive = T)
  dir.create(raw_dir, recursive = T, showWarnings = F)

  file.copy(from = metadata_files, to = raw_dir)

}
