


#' Set File Pattern
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
set.pattern <- function(name, pattern, group="") {
  require(dplyr)

  df_patterns <- get.patterns()  %>%
    filter(name != {{name}}) %>%
    bind_rows(data.frame(name , pattern, group )) %>%
    distinct()

  save.patterns(df_patterns)

}



pattern.file.name <- function() {
  "file_patterns"
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
  if(file.exists(fname)) file.remove(fname, showWarnings=FALSE)


  file_patterns <- data.frame(name = character(0) , pattern = character(0) , group = character(0),
                              desc = character(0))

  file_patterns  <- file_patterns %>%

    append.pattern("data_folder","./data/",
                   desc ="Standard location of user created data") %>%

    append.pattern("raw_data_folder","./data/",
                   desc ="Standard location of data from another source") %>%

    append.pattern("brfss_url_files","https://www.cdc.gov/brfss/annual_data/[YEAR]/files/",
                   desc ="Base URL of BRFSS data") %>%

    append.pattern("xpt_download_zip_file","LLCP[YEAR]XPT.zip",
                   group ="sas_downloads",
                   desc ="URL filename of BRFSS XPT data") %>%

    append.pattern("sasout_download_file","SASOUT[YR]_LLCP.SAS",
                   group ="sas_downloads",
                   desc ="URL filename of BRFSS SASOUT data") %>%

    append.pattern("sas_format_file", "FORMAT[YR].sas",
                   group ="sas_downloads") %>%

    append.pattern("sas_formas_file", "FORMAS[YR].sas",
                   group ="sas_downloads") %>%

    append.pattern("xpt_download_version","LLCP[YR]V[VERS]_XPT.zip",
                   group ="sas_version_downloads") %>%

    append.pattern("sasout_download_version_file","SASOUT[YR]_LLCP_V[VERS].SAS",
                   group ="sas_version_downloads") %>%

    append.pattern("brfss_url_documentation", "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/") %>%

    append.pattern("brfss_raw_data_folder","./data_raw/") %>%
    append.pattern("brfss_annual_raw_data_folder","./data_raw/[YEAR]/") %>%
    append.pattern("brfss_data_folder","./data/") %>%
    append.pattern("brfss_annual_data_folder","{brfss_data_folder}[YEAR]/") %>%
    append.pattern("brfss_geog_folder","{brfss_data_folder}[YEAR]/geog/") %>%
    append.pattern("brfss_geog_file","[GEOG]_[YEAR].RData") %>%
    append.pattern("brfss_geog_file_version","[GEOG]_[YEAR]_V[VERS].RData") %>%
    append.pattern("brfss_geog_path","{brfss_geog_folder}{brfss_geog_file}") %>%
    append.pattern("brfss_columns_file","{brfss_data_folder}[YEAR]/columns_[YEAR].RData") %>%
    append.pattern("brfss_layout_folder","{brfss_annual_data_folder}") %>%
    append.pattern("brfss_layout_file","layout_[YEAR].RData") %>%
    append.pattern("brfss_layout_path","{brfss_layout_folder}{brfss_layout_file}") %>%
    append.pattern("ascii_raw_data_folder","{brfss_raw_data_folder}[YEAR]/ascii/") %>%
    append.pattern("ascii_downloads","{brfss_url_files}LLCP[YEAR]ASC.zip") %>%
    append.pattern("sas_raw_folder","{brfss_raw_data_folder}[YEAR]/sas/") %>%
    append.pattern("sas_data_folder","{brfss_annual_data_folder}") %>%
    append.pattern("xpt_file", "LLCP[YEAR].XPT") %>%
    append.pattern("xpt_file_version", "LLCP[YR]V[VERS].XPT") %>%
    append.pattern("sas_rdata","xpt_[YEAR].RData") %>%
    append.pattern("sas_rdata_version","xpt_[YEAR]_V[VERS].RData") %>%
    append.pattern("sas_sasout_version", "SASOUT[YR]_LLCP_V[VERS].SAS") %>%
    append.pattern("sas_sasout", "SASOUT[YR]_LLCP.SAS") %>%
    append.pattern("sas_sasout_path", "{sas_raw_folder}{sas_sasout}")

  usethis::use_data(file_patterns, overwrite = TRUE)

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


get.patterns <- function() {

  file_patterns <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {

    data("file_patterns")
    save.patterns(file_patterns)
  }

  file_patterns
}

save.patterns <- function(file_patterns) {

  usethis::use_data(file_patterns, overwrite = TRUE)

}

expand.pattern <- function(pattern) {

  ctr <- 0
  while(grepl("\\{", pattern) && ctr<10) {
    pat0 <- gsub(".*?\\{(.*?)\\}.*","\\1",pattern)
    pat1<- get.pattern(pat0)
    pattern <- gsub(paste0("(\\{",pat0,"\\})"),pat1,pattern)
    ctr <- ctr + 1

  }
  pattern
}

get.pattern <- function(name_in) {
  require(dplyr,quietly = T, warn.conflicts = F)

  file_pattern <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {

    data("file_patterns")
    save.patterns(file_patterns)
  }


  file_pattern %>% filter(name==name_in) %>%
    pull(pattern)

}

get.pattern.group <- function(name_in) {
  require(dplyr,quietly = T, warn.conflicts = F)

  file_pattern <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {

    data("file_patterns")
    save.patterns(file_patterns)
  }


  file_pattern %>% filter(group==name_in) %>%
    pull(pattern)

}

get.pattern.info <- function(name_in) {
  require(dplyr,quietly = T, warn.conflicts = F)

  file_pattern <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {

    data("file_patterns", package = "brfss")
    save.patterns(file_patterns)
  }


  file_pattern %>% filter(group==name_in) %>%
    pull(pattern)

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
#' \dontrun {
#' sasout_file<-apply.pattern("sas_sasout_version",YEAR = year, VERS = version)
#'
#' }
#'
apply.pattern <- function(name,  ...) {
  require(dplyr,quietly = T, warn.conflicts = F)

  pats <- get.patterns() %>%
    filter(name == {{name}}) %>%
    pull(pattern)
  #
  pats <- sapply(pats, function(pat) {
    #pat <- get.pattern(name)
    pat <- expand.pattern(pat)

    pat
  })
  pats <- unname(pats)

  patternize(pats,... )

}

################################

patternize<-function(strIn, ...) {

  args <- list(...)

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


  ret
}
