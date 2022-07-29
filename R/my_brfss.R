
#' BRFSS Geography/Years of Interest
#'
#' Set the current default parameters for working with BRFSS data
#'
#' @param year int vector of year(s) to set as default
#' @param geog character vector of geography abbreviations to set as default
#' @param other_geogs character vector of other geography abbreviations to process
#' @param source character data source to process, currently "sas" or "ascii"
#' @param extent character data source extent, currently "local" or "national"
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
my.brfss <- function(year= NULL, geog= NULL, other_geogs= NULL, source = NULL, extent = NULL) {

  if(!is.null(source)) source<-match.arg(source,c("sas","ascii"))
  if(!is.null(extent)) extent<-match.arg(extent,c("local","national"))

  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rds")

  if(file.exists(path)) {
    readRDS(file = path)
    if(is.null(extent)) extent <- my_brfss$extent
    if(is.null(source)) source <- my_brfss$source
    if(is.null(geog)) geog <- my_brfss$geog
    if(is.null(year)) year <- my_brfss$year
    if(is.null(other_geogs)) other_geogs <- my_brfss$other_geogs
  } else {
    if(is.null(extent)) extent <- my.brfss.default.extent()
    if(is.null(source)) source <- my.brfss.default.source()
    if(is.null(other_geogs)) geog <- my.brfss.default.other.geogs()
    if(is.null(geog)) geog <- my.brfss.default.geog()
    if(is.null(year)) year <- my.brfss.default.year()

  }
  my_brfss <- list(year=year, geog = geog, other_geogs = other_geogs, source = source, extent = extent)

  saveRDS(my_brfss,file = path)
  my_brfss
}

#' BRFSS Geography of Interest
#'
#' Get the geography of interest for working with BRFSS data
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

#' BRFSS Data Source
#'
#' Get the source for the BRFSS data of interest
#'
#'
#'
#' @return character data source ('sas' or 'ascii')
#' @export
#'
#' @examples
#' \dontrun{
#' my.source()
#'
#' }
#'
my.source <- function() {
  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(!file.exists(path)) {
    return(my.brfss.default.source())

  } else {

    load(file = path)
    x <- my_brfss$source
    names(x) <- "source"
    return(x)
  }
}

#' BRFSS Data Source Extent
#'
#' Get the extent for the BRFSS data of interest. The extent is either 'local' or 'national'. Generally, data
#' downloaded from the publicly available data set are 'national' since they cpontains data from all geographies,
#' and data downloaded from the secure CDC state-specific site are 'local' since they only contains data for that
#' geography (state or territory)
#'
#'
#'
#' @return character data extent ('local' or 'national')
#' @export
#'
#' @examples
#' \dontrun{
#' my.extent()
#'
#' }
#'
my.extent <- function() {
  folder <- apply.pattern("brfss_data_folder")
  path <- paste0(folder, "my_brfss.rda")

  if(!file.exists(path)) {
    return(my.brfss.default.extent())

  } else {

    load(file = path)
    x <- my_brfss$extent
    names(x) <- "extent"
    return(x)
  }
}

my.brfss.default.source <- function() { "sas" }

my.brfss.default.extent <- function() { "local" }

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

##  gets the my_brfss source if source is missing or NULL

get.source <- function(source = NULL) {


  if(is.null(source)) source <- my.source()

  source

}

##  gets the my_brfss extent if extent is missing or NULL

get.extent <- function(extent = NULL) {


  if(is.null(extent)) extent <- my.extent()

  extent

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

