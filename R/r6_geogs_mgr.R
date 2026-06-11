
#' GeogMgr R6 Class
#'
#' @export
GeogMgr <-
  R6::R6Class(
    classname = "GeogMgr",

    private = list(
      df_geogs = NULL,
      my_geog = NULL,

      is_numberish = function(x) {

        if(is.numeric(x)) return(TRUE)

        x2 <- gsub("[0-9]*","", x)

        return(nchar(x2) == 0)
      },

      is_fips = function(x) {

        if(!private$is_numberish(x)) return(FALSE)

        x <- as.integer(x)
        return(x %in% private$df_geogs$fips)
      },

      is_abbrev = function(x) {

        if(is.character(x) && nchar(x) == 2) {
          x <- toupper(x)
          return(x %in% private$df_geogs$abbr)
        } else {
          return(FALSE)
        }
      },

      valid_name = function(x) {

        if(is.character(x) && nchar(x) > 2) {
          return(grepl(x, private$df_geogs$name))
        } else {
          return(FALSE)
        }

      }
    ),

    public = list(

      my_geog_filename = "my_geog.rds",

      initialize = function(geog = NULL, geogs_path = NULL) {

        fm <- FileMgr$new(simple = TRUE)
        geogs_path <- fm$apply("geogs_path")

        private$df_geogs <- self$geogs_data()

        # set default geog

        if(is.null(geog)) {
          file <- paste0(fm$apply("data_folder"), self$my_geog_filename)
          my_geog <- readRDS(file)
        } else {
          my_geog <- geog
        }
        self$abbrev <-  my_geog
      },

      geogs_data = function() {

        e <- new.env()

        data("geogs", package = "brfss", envir = e)
        get(ls(e),envir = e)

      },

      join_by = function(df, fips = NULL, abbrev = NULL, name = NULL){

        fips <- fips %||% "fips"

        if(!fips %in% df %>% colnames()) {
          message(paste0(fips, "not a column in df"))
          return(df)
        }
        p <- private

        browser()
        df <- df %>% join_left(p$df_geogs )

      }


    ),

    active = list(

      fips = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$my_geog$fips)
        }

        if(!p$is_fips(value)) {

          message("invalid fips code")
          return()
        }

        p$my_geog <- p$df_geogs %>% filter(as.integer(fips) == value)

      },

      ifips = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(as.integer(p$my_geog$fips))
        }

        if(!p$is_fips(value)) {

          message("invalid fips code")
          return()
        }

        p$my_geog <- p$df_geogs %>% filter(as.integer(fips) == value)

      },

      abbrev = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$my_geog$abbr)
        }

        if(!p$is_abbrev(value)) {

          message(paste0("'", value, "' is an invalid abbreviation"))
          return()
        }

        p$my_geog <- p$df_geogs %>% filter(abbr == value)

      },

      name = function(value){

        p <- private

        if(missing(value)) {

          if(nrow(p$my_geog) != 1) {
            message("no geog has been requested")
            return(NULL)
          }
          return(p$my_geog$name)
        }

        if(!p$valid_name(value)) {

          message("invalid state name")
          return()
        }

        p$my_geog <- p$df_geogs %>% filter(grepl(value, name, ignore.case = T))

      },

      geogs = function(value) {

        if(!missing(value)) {
          message("property is read-only")
          return()
        }

        return(private$df_geogs)
      },

      geog = function(value) {

        if(!missing(value)) return()

        private$my_geog
      }

    )

  )

#' @export
GeogMgr$as_fips <- function(x) {

  if(x %>% gsub("[0-9]", "", .) == "") return(x)

  gm <- GeogMgr$new()
  df <- geog_mgr$geogs %>% filter(abbr == x)

  if(nrow(df) == 1) return(df %>% pull(fips))

  return(NULL)

}

#' @export
GeogMgr$geogs <- function() {

  GeogMgr$new()$geogs

}


#' @export
GeogMgr$as_abbrev <- function(x) {

  if(x %>% gsub("[0-9]", "", .) != "") return(NULL)

  x <- as.integer(x) %>% sprintf("%02d", .)

  gm <- GeogMgr$new()
  df <- geog_mgr$geogs %>% filter(fips == x)

  if(nrow(df) == 1) return(df %>% pull(abbr))

  return(NULL)

}

