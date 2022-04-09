


#' Set Naming Pattern
#'
#' If the name exists already, it will overwrite (update) that name without warning.
#' If the name does not exist, it will add it.
#'
#'
#' @param name character: name of pattern
#' @param pattern
#' @param group
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' set.pattern(name = "raw_data_folder", pattern = "./data_raw/", desc = "Folder for raw BRFSS data")
#' }
#'
set.pattern <- function(name, pattern= NULL, group="", desc = "") {
  require(dplyr)

  df_patterns <- get.patterns()

  if(is.null(pattern)) {
    return(NULL)
  }

  df_patterns <- df_patterns %>%
    filter(name != {{name}}) %>%
    bind_rows(data.frame(name , pattern, group, desc )) %>%
    distinct()

  save.patterns(df_patterns)

}



pattern.file.name <- function() {
  "./data/file_patterns.rda"
}

append.pattern <- function(df, name , pattern , group = "", desc = "") {

  #df0 <- data.frame(name  , pattern , group, desc)

  df %>%
    tibble::add_row( name, pattern , group, desc)
}

init.patterns <- function() {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tibble)

  fname <- "./data/file_patterns.rda"
  if(file.exists(fname)) file.remove(fname)


  file_patterns <- data.frame(name = character(0) , pattern = character(0) , group = character(0),
                              desc = character(0))

  file_patterns  <- file_patterns %>%

    append.pattern("data_folder","./data/",
                   desc ="Standard location of user created data") %>%

    append.pattern("raw_data_folder","./data_raw/",
                   desc ="Standard location of data from another source") %>%

    append.pattern("output_folder","./output/",
                   desc ="Standard location of data, files, or reports for sharing") %>%

    append.pattern("brfss_url_files","https://www.cdc.gov/brfss/annual_data/[YEAR]/files/",
                   desc ="Base URL of BRFSS data") %>%

    append.pattern("sasout_download_file","SASOUT[YR]_LLCP.SAS",
                   group ="sas_downloads",
                   desc ="URL filename of BRFSS SASOUT data") %>%

    append.pattern("sas_format_file", "FORMAT[YR].sas",
                   group ="sas_downloads",
                   desc = "SAS Format library file") %>%

    append.pattern("sas_formas_file", "FORMAS[YR].sas",
                   group ="sas_downloads",
                   desc = "SAS Format assignment file") %>%

    append.pattern("xpt_download_zip_file","LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]_)XPT.zip",
                   group ="xpt_downloads",
                   desc ="URL filename of BRFSS XPT data") %>%

    append.pattern("brfss_url_documentation", "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/") %>%

    append.pattern("brfss_raw_data_folder","{raw_data_folder}",
                   desc = "Folder to store raw (imported/downloaded from CDC) data") %>%

    append.pattern("brfss_annual_raw_data_folder","{brfss_raw_data_folder}[YEAR]/",
                   desc = "Folder to store the annual raw (imported/downloaded from CDC) data") %>%

    append.pattern("brfss_data_folder","{data_folder}",
                   desc = "Folder to store processed BRFSS data") %>%

    append.pattern("brfss_annual_data_folder","{brfss_data_folder}[YEAR]/",
                   desc = "Folder to store the annual processed BRFSS data") %>%

    append.pattern("brfss_geog_folder","{brfss_data_folder}[YEAR]/geog/[GEOG]/",
                   desc = "Folder to store the annual processed BRFSS data from specific geographies") %>%

    append.pattern("brfss_geog_file","[GEOG]_[YEAR]([VERS] > 0;_V[VERS]).RData",
                   desc = "File name for annual processed BRFSS data (main survey) from specific geographies") %>%

    append.pattern("brfss_geog_path","{brfss_geog_folder}{brfss_geog_file}",
                   desc = "Full path for annual processed BRFSS data (main survey) from specific geographies") %>%

    #############################################################################
  ##
  ##  codebook patterns

  append.pattern("brfss_url_codebook",
                 "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/",
                 desc ="Base URL of BRFSS codebook") %>%

    append.pattern("brfss_codebook_file1",
                   "{brfss_url_codebook}codebook[YR]_llcp.pdf",
                   group ="codebook_downloads",
                   desc ="Pre-2017 codebook name") %>%

    append.pattern("brfss_codebook_file2",
                   "{brfss_url_codebook}codebook[YR]_llcp-v2-508.pdf",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name") %>%

    append.pattern("brfss_codebook_file3",
                   "{brfss_url_codebook}codebook[YR]_llcp-v2-508.HTML",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name in html") %>%

    append.pattern("codebook_folder","{brfss_annual_raw_data_folder}",
                   desc = "location of the annual codebook file") %>%

    append.pattern("codebook_file","codebook[YR]_llcp",
                   desc = "file name of the annual codebook file") %>%

    append.pattern("codebook_ext","pdf",
                   desc = "ext of the annual codebook file (.txt, .rtf, or .pdf") %>%

    append.pattern("codebook_layout_folder","{sas_layout_folder}",
                   desc = "location of the annual codebook layout file") %>%

    append.pattern("codebook_layout_file","layout[YR]_CB.RData",
                   desc = "file name of the annual codebook layout file") %>%

    append.pattern("codebook_layout_path","{codebook_layout_folder}{codebook_layout_file}",
                   desc = "path to the annual codebook layout file") %>%

    #########################################################################################

  append.pattern("sas_layout_folder","{brfss_annual_data_folder}layout/") %>%
    append.pattern("sas_layout_file","layout[YR]_sas.RData") %>%
    append.pattern("sas_layout_path","{sas_layout_folder}{sas_layout_file}") %>%
    # append.pattern("brfss_columns_folder","{brfss_annual_data_folder}layout/") %>%
    # append.pattern("brfss_columns_file","columns_[YEAR].RData") %>%
    # append.pattern("brfss_columns_path","{brfss_columns_folder}{brfss_columns_file}") %>%

    ##    data file of responses

    append.pattern("brfss_responses_folder","{brfss_annual_data_folder}") %>%
    append.pattern("brfss_responses_file","responses_[YEAR].RData") %>%
    append.pattern("brfss_responses_path","{brfss_responses_folder}{brfss_responses_file}") %>%

    append.pattern("brfss_modules_folder","{brfss_annual_data_folder}") %>%
    append.pattern("brfss_modules_file","modules_[YEAR].RData") %>%
    append.pattern("brfss_modules_path","{brfss_modules_folder}{brfss_modules_file}") %>%

    ##  ascii data

    append.pattern("ascii_filename_zip","LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]_)ASC.zip") %>%
    #
    append.pattern("ascii_downloads_url","{brfss_url_files}{ascii_filename_zip}",
                   group = "ascii_downloads") %>%

    append.pattern("ascii_raw_data_folder","{brfss_raw_data_folder}[YEAR]/ascii/") %>%

    append.pattern("ascii_filename_raw","LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]).ASC") %>%

    append.pattern("ascii_path_zip","{ascii_raw_data_folder}{ascii_filename_zip}") %>%

    append.pattern("ascii_path_raw","{ascii_raw_data_folder}{ascii_filename_raw}") %>%

    append.pattern("ascii_data_folder","{brfss_data_folder}[YEAR]/ascii/") %>%
    append.pattern("ascii_filename","LLCP[YEAR]ASC([VERS] > 0;_V[VERS]).rda") %>%
    append.pattern("ascii_path","{ascii_data_folder}{ascii_filename}") %>%

    ###################################################################################

  append.pattern("sas_raw_folder","{brfss_raw_data_folder}[YEAR]/sas/") %>%
    append.pattern("sas_data_folder","{brfss_annual_data_folder}xpt/") %>%
    append.pattern("xpt_folder", "{sas_raw_folder}") %>%
    append.pattern("xpt_file", "LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]).XPT") %>%
    append.pattern("xpt_path", "{xpt_folder}{xpt_file}") %>%

    append.pattern("sas_data_file","xpt_[YEAR]([VERS] > 0;_V[VERS]).RData") %>%
    append.pattern("sas_data_path","{sas_data_folder}{sas_data_file}") %>%

    append.pattern("sas_sasout", "SASOUT[YR]_LLCP([VERS] > 0;_V[VERS]).SAS") %>%
    append.pattern("sas_sasout_path", "{sas_raw_folder}{sas_sasout}") %>%
    append.pattern("saq_layout_raw","{brfss_annual_raw_data_folder}SAQ[YR].csv") %>%
    append.pattern("saq_layout","{brfss_annual_data_folder}saq_layout[YEAR].rda")

  usethis::use_data(file_patterns, overwrite = TRUE)

}

#' Move File to Standard Location
#'
#' Move a file (using file.rename()) to aa location specified in the standard (expected)
#'  location using pattern naming.
#'
#' @param name character pattern name
#' @param file character file to be renamed
#' @param ... value pairs to be passed to the apply.pattern function
#'
#' @return logical status of operation
#' @export
#'
#' @examples
#' \dontrun{
#' relocate_by_pattern("ascii_path_raw", "./temp/ascii.dat", YEAR = 2021, VERS = 0)
#' }
relocate_by_pattern <- function(name, file, ...) {

  path_out <- apply.pattern(name, ...)

  dir_out <- dirname(path_out)

  if(!dir.exists(dir_out)) dir.create(dir_out)

  file.rename(from = file, to = path_out)
}


remove.patterns <- function(names) {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)

  df_patterns <- get.patterns() %>%
    filter(!name %in% names) %>%
    distinct()

  save.patterns(df_patterns)

}

#' File Pattern Names
#'
#' @param filter pattern (see grep) to apply to names
#'
#' @return character vector of pattern names
#' @export
#'
#' @examples
#' \dontrun{
#' # get file pattern names that have the word folder in them
#' pattern.names(filter = "folder")
#' }
#'
pattern.names <- function(filter=".*") {
  require(dplyr)

  get.patterns() %>%
    filter(grepl(filter,name)) %>%
    pull(name)
}


#' Reset File Pattern Data
#'#'
#' @export
#'
refresh.patterns <- function() {

  file_patterns <- pattern.file.name()
  if(file.exists(file_patterns)) {
    file.remove(file_patterns)
  }
  env <- new.env()
  data("file_patterns", package = "brfss", envir = env )

  file_patterns <- get(ls(env),envir = env)
  save.patterns(file_patterns)

  invisible()
}

#' Get File Pattern Data
#'#'
#' @return data frame with all pattern data
#' @export
#'
get.patterns <- function() {

  file_patterns <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {
    env <- new.env()
    data("file_patterns", package = getPackageName(), envir = env )

    file_patterns <- get(ls(env),envir = env)
    save.patterns(file_patterns)
  }

  file_patterns
}

save.patterns <- function(file_patterns) {

  suppressMessages(
    usethis::use_data(file_patterns, overwrite = TRUE)
  )

}

expand.pattern <- function(pattern) {

  ctr <- 0
  while(grepl("\\{", pattern) && ctr<10) {
    pat0 <- gsub(".*?\\{(.*?)\\}.*","\\1",pattern)
    pat1<- get.pattern(pat0)
    pattern <- gsub(paste0("(\\{",pat0,"\\})"),pat1,pattern)
    ctr <- ctr + 1

  }

  # get rid of double slashes (usually from substitution of other patterns)

  pattern <- gsub("([^:])//","\\1/",pattern)

  pattern
}

#' Get File Pattern
#'
#' Get the file pattern for a given name. If the name does not exist, it will return NULL
#'
#'
#' @param name character - name of pattern
#'
#' @return character - the requested pattern
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern(name = "raw_data_folder")
#' }
#'
get.pattern <- function(name) {
  require(dplyr,quietly = T, warn.conflicts = F)

  pat_name <- name

  get.patterns() %>%
    filter(name=={{pat_name}}) %>%
    pull(pattern)

}

#' Get File Pattern Group
#'
#' Get the file patterns for a given group. If the group does not exist, it will return NULL
#'
#'
#' @param group character: name of pattern group
#'
#' @return named character - the requested patterns with respective names
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern.group(group = "sas_downloads")
#' }
#'
get.pattern.group <- function(group) {
  require(dplyr,quietly = T, warn.conflicts = F)

  file_patterns <- get.patterns()

  file_patterns <- file_patterns  %>% filter(group=={{group}})

  patterns <- file_patterns %>%
    pull(pattern)

  names(patterns) <- file_patterns %>%
    pull(name)


  patterns

}

#' Get File Pattern Info
#'
#' Get the file pattern info (all fields) for a given name. If the name does not exist, it will return NULL
#'
#'
#' @param name character - name of pattern
#'
#' @return data.frame - the requested pattern info/fields (1 row)
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern(name = "raw_data_folder")
#' }
#'
get.pattern.info <- function(name) {
  require(dplyr,quietly = T, warn.conflicts = F)

  file_patterns <- get.patterns()

  file_patterns %>% filter(name=={{name}})

}

#' Apply Variables to File Pattern
#'
#' User passes a pattern to use and the variable/value pairs to insert where specified in the pattern.
#' For example, if you store your annual data under the data folder by year, you might have a pattern named
#' 'annual_data_folder' that is './data/[YEAR]/' and you would call
#' apply.pattern("annual_data_folder", YEAR = 2020)
#'
#' There is one special case. If you pass YEAR as a variable, it will also create YR, and the
#' correesponding 2-digit value.
#'
#' @param name
#' @param ... variables to insert
#'
#' @return character vector with variables inserted where needed
#' @export
#'
#' @examples
#' \dontrun{
#' sasout_file<-apply.pattern("sas_sasout_version",YEAR = year, VERS = version)
#'
#' }
#'
apply.pattern <- function(name,  ...) {
  require(dplyr,quietly = T, warn.conflicts = F)

  pats <- get.pattern(name = name)

  pats <- sapply(pats, function(pat) {
    #pat <- get.pattern(name)
    expand.pattern(pat)
  })

  pats <- unname(pats)

  patternize(pats,... )

}

#############################################

#' Substitute Variables in Pattern
#'
#' @param strIn character - patterns to apply the variables to
#' @param ... list - variables to insert in form of c(YEAR = xxxx, VERS = 1), etc
#'
#' @return character vector of modified strings
#' @export
#'
#' @examples
#' year <- 2020
#' geog <- "MT"
#' patternize("XPT_[GEOG]_[YEAR]", YEAR = year, GEOG = geog)
#'
patternize<-function(strIn, ...) {

  args <- list(...)

  ##  remove args with NULL value

  args <- args[!sapply(args,is.null)]

  nms <- names(args)
  vals <- unlist(unname(args))

  ###########################################################################
  ##
  ##  this is a special case for YEAR ... creating the 2-digit year as well
  yr_arg <- which(nms == "YEAR")

  if(length(yr_arg)>0) {
    year <- as.integer(vals[yr_arg[1]])
    nms[length(nms)+1] <- "YR"
    vals[length(vals)+1] <- year%%100

  }

  #############################################################################
  ret <- strIn

  mapply(function(nm,val) {
    nm <- paste0("[",nm,"]")
    ret<<-gsub(nm,val,ret,fixed = T)


  }, nms, vals)

  #######################################################################
  ##
  ##    check for logical expressions
  ##    e.g. "LLCP([VERS]==0;[YEAR])([VERS]>0;[YR]V[VERS])_XPT.zip"

  #check <- grepl(".*[(](.*)[)].*",ret)

  while(grepl(".*[(](.*)[)].*",ret)) {

    expr <- gsub(".*[(](.*)[)].*","\\1",ret)

    expr <-  gsub("(.*);(.*)","\\1",expr)
    ok <- eval(parse(text = expr))

    if (ok) {
      ret <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\2\\3",ret)
    } else {
      ret <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\3",ret)
    }

    check <- grepl(".*[(](.*)[)].*",ret)

  }

  ret
}

