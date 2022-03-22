




#' BRFSS Geography/Years of Interest
#'
#' Set the default geography and year for working with BRFSS data
#'
#' @param geog character vector of geography abbreviations to set as default
#' @param year int vector of year(s) to set as default
#'
#' @return list with geography and year
#' @export
#'
#' @examples
#' \dontrun{
#' my.brfss(geog="MT", year=2021)
#' my.brfss(geog="MT")
#'
#' }
#'
my.brfss <- function(geog= NULL, year= NULL) {

  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(file.exists(path)) {
    load(file = path)
    if(is.null(geog)) geog <- my_brfss$geog
    if(is.null(year)) year <- my_brfss$year
  } else {
    if(is.null(geog)) geog <- my.brfss.default.geog()
    if(is.null(year)) year <- my.brfss.default.year()

  }
  my_brfss <- list(geog=geog,year=year)

  save(my_brfss,file = path)
  my_brfss
}

#' BRFSS Geography of Interest
#'
#' Get the default geographyfor working with BRFSS data
#'
#' @return character vector of default geometry(s)
#' @export
#'
#' @examples
#' \dontrun{
#' my.geog("MT")
#'
#' }
#'
my.geog <- function() {
  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(!file.exists(path)) {
    return("*")

  } else {

    load(file = path)
    return(my_brfss$geog)
  }
}

my.brfss.default.geog <- function() { "*" }

my.brfss.default.year <- function() {
  month <- as.integer(substr(as.character(Sys.Date()),6,7))
  year <- as.integer(substr(as.character(Sys.Date()),1,4)) - 1
  if(month<5) year <- year - 1
  return(year)
}

#' BRFSS Year(s) of Interest
#'
#' @return integer vector of default year(s)
#' @export
#'
#' @examples
my.year <- function() {
  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(!file.exists(path)) {

    return(my.brfss.default.year())

  } else {

    load(file = path)
    return(my_brfss$year)
  }
}


