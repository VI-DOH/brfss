

save.geogs <- function(geogs) {
  file <-paste0(get.pattern("data_folder"),"geogs.rda")
  save(geogs, file = file)
}

#' Get All Geographies
#'
#' Get the data frame containing all geographies
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_geogs_all()
#' }
get_geogs_all <- function() {

  data("geogs", package="brfss")
  geogs
}


#' Get Geography Name
#'
#' Get the name variable for a geography based on the numeric id or the abbreviation.
#'
#' @param geog integer or character - either the 2-character geography abbreviation or the FIPS ID
#'
#' @return character name of geography (e.g "Montana" or "Virgin Islands")
#' @export
#'
#' @examples
#' \dontrun{
#' # For Alabama  ...
#' geog_name(1)
#' # or
#' geog_name("AL")
#' }
geog_name <- function(geog) {

  geogs <- get_geogs_all()

  if(is.character(geog) && nchar(geog)==2) {
    geog <- geogs %>%
      filter(Abbrev == {{geog}}) %>%
      pull(Geog)

  } else if(is.numeric(geog)) {
    geog <- geogs %>%
      filter(Id == {{geog}}) %>%
      pull(Geog)

  }

  return (geog)
}

#' Get Geography Abbreviation
#'
#' Get the abbreviation variable for a geography based on the numeric id or the full name
#'
#' @param geog integer or character - either the full geography name or the FIPS ID
#'
#' @return character abbreviation of geography (e.g "MT" or "VI")
#' @export
#'
#' @examples
#' \dontrun{
#' # For Montana  ...
#' geog_abb(30)
#' # or
#' geog_abb("Montana")
#' }
#'
geog_abb <- function(geog) {

  geogs <- get_geogs_all()

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- geogs %>%
      filter(Geog == {{geog}}) %>%
      pull(Abbrev)

  } else if(is.numeric(geog)) {
    geog <- geogs %>%
      filter(Id == {{geog}}) %>%
      pull(Abbrev)

  }

  return (geog)
}

#' Get Geography Numeric (FIPS) ID
#'
#' Get the id variable for a geography based on the full name or the abbreviation for the geography.
#'
#' @param geog character - either the full geography name or the 2-character abbreviation
#'
#' @return integer - id of geography
#' @export
#'
#' @examples
#' \dontrun{
#' # For Montana  ...
#' geog_id("MT")
#' # or
#' geog_id("Montana")
#' }
#'
geog_id <- function(geog) {

  geogs <- get_geogs_all()

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- geogs %>%
      filter(Geog == {{geog}}) %>%
      pull(Id)

  } else if(is.character(geog)) {
    geog <- geogs %>%
      filter(Abbrev == {{geog}}) %>%
      pull(Id)

  }

  return (as.integer(geog))
}
