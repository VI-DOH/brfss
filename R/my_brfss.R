




#' BRFSS Geography/Years of Interest
#'
#' Set the default geography and year for working with BRFSS data
#'
#' @param year int vector of year(s) to set as default
#' @param geog character vector of geography abbreviations to set as default
#' @param other_geogs character vector of other geography abbreviations to process
#'
#' @return list with year, geography and other geographies (year, geog, other_geogs)
#' @export
#'
#' @examples
#' \dontrun{
#' my.brfss(geog="MT", year=2021)
#' my.brfss(geog="MT", other_geogs)
#'
#' }
#'
my.brfss <- function(year= NULL, geog= NULL, other_geogs= NULL) {

  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(file.exists(path)) {
    load(file = path)
    if(is.null(geog)) geog <- my_brfss$geog
    if(is.null(year)) year <- my_brfss$year
    if(is.null(other_geogs)) other_geogs <- my_brfss$other_geogs
  } else {
    if(is.null(other_geogs)) geog <- my.brfss.default.other.geogs()
    if(is.null(geog)) geog <- my.brfss.default.geog()
    if(is.null(year)) year <- my.brfss.default.year()

  }
  my_brfss <- list(year=year, geog = geog, other_geogs = other_geogs)

  save(my_brfss,file = path)
  my_brfss
}

#' BRFSS Geography of Interest
#'
#' Get the default geography for working with BRFSS data
#'
#' @return character vector of default geography(s)
#' @export
#'
#' @examples
#' \dontrun{
#' my.geog()
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
    x <- my_brfss$geog
    names(x) <- "geog"
    return(x)
  }
}

#' BRFSS Other Geographies of Interest
#'
#' Get the other geographies for working with BRFSS data. A region might be of interest,
#' rather than a single state. So other geographies might be c("MT","WY", "ID", "ND", "SD")
#'
#' @return character vector of default geometry(s)
#' @export
#'
#' @examples
#' \dontrun{
#' my.other.geogs()
#'
#' }
#'
my.other.geogs <- function() {
  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(!file.exists(path)) {
    return("*")

  } else {

    load(file = path)

    x <- my_brfss$other_geogs
    names(x) <- rep("other_geogs", length(x))

    return(x)
  }
}

my.brfss.default.geog <- function() { "*" }

my.brfss.default.other.geogs <- function() { "" }

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

##  gets the my_brfss year if year is missing or NULL

get.year <- function(year = NULL) {

  if(is.null(year)) year <- my.year()
  year

}

##  gets the my_brfss geog if geog is missing or NULL

get.geog <- function(geog = NULL) {


  if(is.null(geog)) geog <- my.geog()

  geog

}

##  gets the my_brfss other_geogs if other_geogs is missing or NULL

get.other.geogs <- function(other_geogs = NULL) {


  if(is.null(other_geogs)) other_geogs <- my.other.geogs()

  other_geogs

}

##  gets all geogs if geogs is missing or NULL
##  concatenates the geog and other_geogs members of my_brfss

get.geogs <- function(geogs = NULL, named = FALSE) {


  if(is.null(geogs)) {

    ## passing NULL to the following functions automatically gets the my_brfss members

    geogs <- unlist(c(get.geog(NULL), get.other.geogs(NULL)))

    if(!named) geogs <- unname(geogs)

    geogs <- geogs[length(geogs)>0]
    geogs <- geogs[nchar(geogs)>0]
  }

  geogs

}

