
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

      initialize = function() {

        fm <- FileMgr$new()
        geogs_path <- fm$apply("geogs_path")
        private$df_geogs <- readRDS(geogs_path)

        # set default geog

        file <- paste0(fm$apply("brfss_data_folder"), self$my_geog_filename)
        my_geog <- readRDS(file)

        self$abbrev <-  my_geog
      },

      join_by = function(df, fips = NULL, abbrev = NULL, name = NULL){

        fips <- fips %||% "fips"
        browser()
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

          message("invalid abbreviation")
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
