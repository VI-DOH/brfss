

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

  pttrns <- my.brfss.patterns()

  geogs_path <- apply.pattern("geogs_path", pttrns)

  if(!file.exists(geogs_path)) {
    data("geogs", package="brfss")
    saveRDS(geogs, file = geogs_path)
  } else {

    geogs <- readRDS(geogs_path)
  }

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
geog_name <- function(geogs) {

  geogs <- unlist(geogs)

  df_geogs <- get_geogs_all() %>%
    mutate(Id = as.integer(Id))

  if(is.factor(geogs) | is.character(geogs) ) {
    geogs <- tolower(as.character(geogs))
  } else  if(orrr::is.integer_like(geogs)) {
    geogs <- as.integer(geogs)
  }


  df <- data.frame(geog = geogs)


  if(is.character(geogs[1])) {
    if(nchar(geogs[1])==2) {
      geogs <- df %>%
        left_join(df_geogs %>% mutate(Abbrev = tolower(Abbrev)), by=c("geog" = "Abbrev")) %>%
        pull(Geog)
    } else {
      geogs <- stringr::str_to_title(geogs) %>% gsub(" Of ", " of ", .)
    }
  } else if(is.numeric(geogs[1])) {
    geogs <- df %>%
      left_join(df_geogs, by=c("geog" = "Id")) %>%
      pull(Geog)

  }

  geogs


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
geog_abb <- function(geogs) {
  require(dplyr)

  df_geogs <- get_geogs_all() %>%
    mutate(Id = as.integer(Id))

  if(is.factor(geogs) | is.character(geogs) ) {
    geogs <- tolower(as.character(geogs))
  } else  if(orrr::is.integer_like(geogs)) {
    geogs <- as.integer(geogs)
  }

  df <- data.frame(geog = geogs)


  if(is.character(geogs[1]) && nchar(geogs[1])!=2) {
    geogs <- df %>%
      left_join(df_geogs %>% mutate(Geog = tolower(Geog)), by=c("geog" = "Geog")) %>%
      pull(Abbrev)

  } else if(is.numeric(geogs[1])) {
    geogs <- df %>%
      left_join(df_geogs, by=c("geog" = "Id")) %>%
      pull(Abbrev)

  }

  toupper(geogs)


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
geog_id <- function(geogs) {

  df_geogs <- get_geogs_all() %>%
    mutate(Id = as.integer(Id))

  if(is.factor(geogs) | is.character(geogs) ) {
    geogs <- tolower(as.character(geogs))

  } else  if(orrr::is.integer_like(geogs))
  {
    geogs <- as.integer(geogs)
  }


  df <- data.frame(geog = geogs)


  if(is.character(geogs[1]) && nchar(geogs[1])!=2) {
    geogs <- df %>%
      left_join(df_geogs %>% mutate(Geog = tolower(Geog)), by=c("geog" = "Geog")) %>%
      pull(Id)

  } else if(is.character(geogs[1])) {
    geogs <- df %>%
      left_join(df_geogs %>% mutate(Abbrev = tolower(Abbrev)), by=c("geog" = "Abbrev")) %>%
      pull(Id)

  }

  as.integer(geogs)

}

