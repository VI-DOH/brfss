


#' Collect and Filter Data Files from Yearly Subdirectories
#'
#' This function scans a base data directory (determined dynamically using
#' `pattern_params()` and `apply.pattern("data_folder", ...)`) for
#' year-named subdirectories (e.g., "2021", "2022"), lists all files within
#' those directories, and filters the results according to user-specified
#' inclusion (`patterns`) and exclusion (`ignore`) criteria.
#'
#' @param patterns A character vector of regular expressions used to *include* matching files.
#'   If `NULL` (default), all files are included unless excluded by `ignore`.
#' @param ignore A character vector of regular expressions used to *exclude* matching files.
#'   Defaults to `c("monthly", "archive")`.
#'
#' @details
#' The function determines the base data directory dynamically:
#' ```r
#' params <- pattern_params()
#' data_dir <- apply.pattern("data_folder", params)
#' ```
#' This allows integration with user- or environment-specific configurations.
#'
#' Once the data directory is identified, the function:
#' 1. Finds subdirectories named with 4-digit years.
#' 2. Recursively lists all files within them.
#' 3. Removes files matching any of the patterns in `ignore`.
#' 4. Optionally filters the remaining files using `patterns`.
#'
#' @return
#' A character vector of full file paths that satisfy the inclusion/exclusion criteria.
#'
#' @examples
#' \dontrun{
#' # Return all files except those in monthly or archive folders
#' my_data()
#'
#' # Include only files that contain "summary" or "annual" in their names
#' my_data(patterns = c("summary", "annual"))
#'
#' # Exclude "test" files while including only CSVs
#' my_data(patterns = "\\.csv$", ignore = "test")
#' }
#'
#' @seealso [pattern_params()], [apply.pattern()]
#'
#' @importFrom purrr map
#' @export
my_data <- function(patterns = NULL, ignore = c("monthly", "archive")) {

  # Get configuration parameters (user/environment specific)
  params <- pattern_params()

  # Determine base data directory dynamically (e.g., "./data/")
  data_dir <- apply.pattern("data_folder", params)

  # List subdirectories that are 4-digit years (e.g., "2021", "2022")
  years <- list.files(data_dir) %>%
    grep("^[0-9]{4}$", ., value = TRUE) %>%
    as.integer()

  # For each year folder, list all files recursively with full paths
  files <- purrr::map(years, function(year) {
    dir <- paste0(data_dir, year)
    list.files(dir, full.names = TRUE, recursive = TRUE)
  }) %>% unlist()

  # Exclude any files that match patterns in 'ignore'
  # (default: "monthly", "archive")
  if (length(ignore) > 0) {
    for (ig in ignore) {
      files <- grep(ig, files, value = TRUE, invert = TRUE)
    }
  }

  # If inclusion patterns are provided, keep only those that match
  if (length(patterns) > 0) {
    for (pat in patterns) {
      files <- grep(pat, files, value = TRUE, perl = TRUE)
    }
  }

  # Return final list of file paths
  files
}

#' Retrieve Local-Level BRFSS Data Files
#'
#' This function is a convenience wrapper around [my_data()] that retrieves
#' locally scoped BRFSS data files. It can optionally limit results by
#' pattern or geographic keyword.
#'
#' @param pattern A character vector of regular expressions used to include
#'   matching files. If `NULL`, all local data files are included.
#' @param geog An optional geographic identifier. If `NULL`, it is determined
#'   by calling [brfss.param()] with the same argument.
#' @param df Logical. If `TRUE`, returns a data frame with `state`, `year`, and `source`
#'   columns parsed from the file names. If `FALSE` (default), returns a
#'   character vector of file paths.
#' @param path Logical. If `TRUE`, returns the full path in the data frame (if df = TRUE) or
#'   file path vector. If `FALSE` (default), returns a character vector of file names only
#'
#' @details
#' The function searches for state-level `.rds` data files whose names
#' follow the pattern `XX_YYYY.rds` (e.g., `MT_2022.rds`).
#' If `df = TRUE`, it parses state abbreviations, years, and source from filenames
#' and returns a deduplicated data frame.
#'
#' @return
#' Either:
#' * A character vector of file paths (default), or
#' * A data frame with columns `state`, `year` and source if `df = TRUE`.
#' @examples
#' \dontrun{
#' # Get all local BRFSS data files
#' my_local_data()
#'
#' # Get local files related to risk factors
#' my_local_data(pattern = "risk")
#'
#' # Get local files for a specific geography
#' my_local_data(geog = "MT")
#' }
#'
#' @seealso [my_data()], [brfss.param()]
#' @export
my_local_data <- function(pattern = NULL, geog = NULL, df = FALSE, path = FALSE) {

  if(is.null(geog)) geog <- my_geog()

  paths<- my_data(c(pattern, geog, "local", "/.._[0-9]{4}.rds"))

  if(df) {
    x <- data.frame(year = gsub(".*/(..)_([0-9]{4}).rds", "\\2", paths),
                    state = gsub(".*/(..)_[0-9]{4}.rds", "\\1", paths),
                    source = gsub(".*/(.*)/(..)_[0-9]{4}.rds", "\\1", paths)) %>%
      distinct()

    if(path) {
      x <- x %>% mutate(full_path = paths)
    } else  {
      x <- x %>% mutate(filename = gsub(".*/(.._[0-9]{4}.rds)", "\\1", paths))
    }
  } else {
    x = paths
    if(!path) x <- gsub(".*/(.._[0-9]{4}.rds)", "\\1", paths)
  }

  x
}

#' Retrieve or Summarize State-Level BRFSS Data Files
#'
#' This function retrieves state-level BRFSS data files using [my_data()],
#' and can optionally return a data frame summarizing the available files
#' by state and year.
#'
#' @param pattern A character vector of regular expressions used to include
#'   matching files. Can be used to search for one or more states(e.g., `MT|ID`If `NULL`,
#'   all state-level files are included.
#' @param df Logical. If `TRUE`, returns a data frame with `state`, `year`, and `source`
#'   columns parsed from the file names. If `FALSE` (default), returns a
#'   character vector of file paths.
#' @param path Logical. If `TRUE`, returns the full path in the data frame (if df = TRUE) or
#'   file path vector. If `FALSE` (default), returns a character vector of file names only
#'
#' @details
#' The function searches for state-level `.rds` data files whose names
#' follow the pattern `XX_YYYY.rds` (e.g., `MT_2022.rds`).
#' If `df = TRUE`, it parses state abbreviations, years, and source from filenames
#' and returns a deduplicated data frame.
#'
#' @return
#' Either:
#' * A character vector of file paths (default), or
#' * A data frame with columns `state`, `year` and source if `df = TRUE`.
#'
#' @examples
#' \dontrun{
#' # Get all state-level BRFSS data file paths
#' my_states_data()
#'
#' # Get only files matching a specific pattern (e.g., "MT|ID")
#' my_states_data(pattern = "risk")
#'
#' # Get a summary of available state-year combinations
#' my_states_data(df = TRUE)
#' }
#'
#' @seealso [my_data()]
#' @importFrom dplyr distinct
#' @export
my_states_data <- function(pattern = NULL, df = FALSE, path = FALSE) {

  paths<- my_data(c(pattern, "states", "/.._[0-9]{4}.rds"))

  if(df) {
    x <- data.frame(year = gsub(".*/(..)_([0-9]{4}).rds", "\\2", paths),
                    state = gsub(".*/(..)_[0-9]{4}.rds", "\\1", paths),
                    source = gsub(".*/(.*)/(..)_[0-9]{4}.rds", "\\1", paths)) %>%
      distinct()

    if(path) {
      x <- x %>% mutate(full_path = paths)
    } else  {
      x <- x %>% mutate(filename = gsub(".*/(.._[0-9]{4}.rds)", "\\1", paths))
    }
  } else {
    x = paths
    if(!path) x <- gsub(".*/(.._[0-9]{4}.rds)", "\\1", paths)
  }

  x
}
