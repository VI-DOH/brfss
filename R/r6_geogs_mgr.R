#' Geographic Manager Class
#'
#' @description
#' Manages and validates regional geospatial configurations, FIPS codes, state abbreviations, and names.
#' It interfaces with localized configuration files and underlying BRFSS geospatial reference data.
#'
#' @export
GeogMgr <-
  R6::R6Class(
    classname = "GeogMgr",

    private = list(
      ..df_geogs = NULL,
      ..my_geog = NULL,

      is_numberish = function(x) {

        if(is.numeric(x)) return(TRUE)

        x2 <- gsub("[0-9]*","", x)

        return(nchar(x2) == 0)
      },

      is_fips = function(x) {

        if(!private$is_numberish(x)) return(FALSE)

        x <- as.integer(x)
        return(x %in% private$..df_geogs$fips)
      },

      is_abbrev = function(x) {

        if(is.character(x) && nchar(x) == 2) {
          x <- toupper(x)
          return(x %in% private$..df_geogs$abbr)
        } else {
          return(FALSE)
        }
      },

      valid_name = function(x) {

        if(is.character(x) && nchar(x) > 2) {
          return(grepl(x, private$..df_geogs$name))
        } else {
          return(FALSE)
        }

      }
    ),

    public = list(

      #' @field my_geog_filename Default name of the saved geometry file. Default is `"my_geog.rds"`.
      my_geog_filename = "my_geog.rds",

      #' @description
      #' Initialize a new Geographic Manager.
      #' @param geog Optional default geography identifier (e.g., FIPS, abbreviation, or name string).
      #' @param geogs_path Optional explicit directory path to geographic assets.
      #' @return A new `GeogMgr` object.
      initialize = function(geog = NULL, geogs_path = NULL) {

        fm <- FileMgr$new(simple = TRUE)
        geogs_path <- fm$apply("geogs_path")

        private$..df_geogs <- self$geogs_data()

        # set default geog

        if(is.null(geog)) {
          file <- paste0(fm$apply("data_folder"), self$my_geog_filename)
          my_geog <- readRDS(file)
        } else {
          my_geog <- geog
        }
        self$abbrev <-  my_geog
      },

      #' @description
      #' Retrieve geospatial boundary lookups from the BRFSS data environment.
      #' @return A data frame containing FIPS, abbreviations, and names.
      geogs_data = function() {

        e <- new.env()

        data("geogs", package = "brfss", envir = e)
        get(ls(e),envir = e)

      },

      #' @description
      #' Left-join an incoming data frame against the baseline reference geography metadata.
      #' @param df The target data frame to expand.
      #' @param fips Optional specific name of the incoming FIPS column identifier. Defaults to `"fips"`.
      #' @param abbrev Optional abbreviation boundary filter.
      #' @param name Optional state name filter.
      #' @return The mutated, joined data frame.
      join_by = function(df, fips = NULL, abbrev = NULL, name = NULL){

        fips <- fips %||% "fips"

        cnames <- df %>% colnames()

        if(!fips %in% cnames) {
          message(paste0(fips, "not a column in df"))
          return(df)
        }
        p <- private

        df <- df %>%
          left_join(p$..df_geogs %>% mutate(fips = as.integer(fips)),
                    by = join_by({{fips}} == fips) )
      }


    ),

    active = list(

      #' @field fips Active binding to get or set the current geography via a standard FIPS code string.
      fips = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$..my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$..my_geog$fips)
        }

        if(!p$is_fips(value)) {

          message("invalid fips code")
          return()
        }

        p$..my_geog <- p$..df_geogs %>% filter(as.integer(fips) == value)

      },

      #' @field ifips Active binding to get or set the current geography via an integer format FIPS code.
      ifips = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$..my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(as.integer(p$..my_geog$fips))
        }

        if(!p$is_fips(value)) {

          message("invalid fips code")
          return()
        }

        p$..my_geog <- p$..df_geogs %>% filter(as.integer(fips) == value)

      },

      #' @field abbrev Active binding to get or set the current geography via a 2-character state abbreviation.
      abbrev = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$..my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$..my_geog$abbr)
        }

        if(!p$is_abbrev(value)) {

          message(paste0("'", value, "' is an invalid abbreviation"))
          return()
        }

        p$..my_geog <- p$..df_geogs %>% filter(abbr == value)

      },

      #' @field name Active binding to get or set the current geography via a state name or regex string matches.
      name = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$..my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$..my_geog$name)
        }

        if(!p$valid_name(value)) {

          message("invalid state name")
          return()
        }

        p$..my_geog <- p$..df_geogs %>% filter(grepl(value, name, ignore.case = T))

      },

      #' @field geogs Read-only baseline master reference dataframe containing all supported global geographies.
      geogs = function(value) {

        if(!missing(value)) {
          message("property is read-only")
          return()
        }

        return(private$..df_geogs)
      },

      #' @field geog Read-only active subset row representing the singular geography configuration currently targeted.
      geog = function(value) {

        if(!missing(value)) return()

        private$..my_geog
      }

    )

  )


#' Retrieve Baseline Geography Data Frame
#'
#' @description
#' Directly instantiates a class instance to query the full internal baseline geographic reference table.
#'
#' @return A data frame containing all registered geographies.
#' @export
GeogMgr$geogs <- function() {

  GeogMgr$new()$geogs

}

#' Convert Input to FIPS Format
#'
#' @description
#' Interprets a state postal code or character variant and maps it into its respective standard geographic FIPS designation.
#'
#' @param x A character abbreviation or numeric tracking element.
#' @return A character representation of a valid FIPS code, or `NULL` if not found.
#' @export
GeogMgr$as_fips <- function(x) {

  if(x %>% gsub("[0-9]", "", .) == "") return(x)

  geog_mgr <- GeogMgr$new()
  df <- geog_mgr$geogs %>% filter(abbr == x)

  if(nrow(df) == 1) return(df %>% pull(fips))

  return(NULL)

}


#' Convert Input to State Abbreviation
#'
#' @description
#' Interprets a numeric or character FIPS assignment code and turns it into a standardized 2-character postal abbreviation.
#'
#' @param x A character or integer numeric string tracking index.
#' @return A 2-character capitalized regional code, or `NULL` if not found.
#' @export
GeogMgr$as_abbrev <- function(x) {

  if(x %>% gsub("[0-9]", "", .) != "") return(NULL)

  x <- as.integer(x) %>% sprintf("%02d", .)

  gm <- GeogMgr$new()
  df <- geog_mgr$geogs %>% filter(fips == x)

  if(nrow(df) == 1) return(df %>% pull(abbr))

  return(NULL)

}


#' Resolve Full Geography Name
#'
#' @description
#' Evaluates ambiguous short codes, integers, abbreviations, or FIPS targets and yields the fully expanded string equivalent title.
#'
#' @param x Character or numeric identifier mapping code.
#' @return The proper full name string of the parsed territory, or `NULL` if unresolved.
#' @export
GeogMgr$full_name <- function(x) {

  gm <- GeogMgr$geogs()

  x <- as.character(x)

  fips <- x %>% gsub("[^0-9]", "", .)

  if(nchar(fips) != 0) {

    df <- gm %>% filter(fips == .env$fips)
    if(nrow(df) == 1) {
      return(df %>% pull(name))
    } else {
      return(NULL)
    }
  }

  if(nchar(x) != 2) return(NULL)

  df <- gm %>% filter(abbr == .env$x)

  if(nrow(df) == 1) return(df %>% pull(name))

  return(NULL)

}

#' Set/Get Default Geography Name
#'
#' @description
#' Gets the default geography name for this user. Normally it will be set once, but if user chooses
#'
#' @param x Character or numeric identifier to identify the default geography. Id missing, this is a getter.
#' @return The 2-character abbreviation of the default geography, or `NULL` if setting.
#' @export
GeogMgr$my_geog <- function(x) {

  file <- GeogMgr$new()$my_geog_filename

  if(missing(x)) {
    if(!file.exists(file)) return(NULL)
    return(readRDS(file))
  }

  saveRDS(x, file = file)
}
