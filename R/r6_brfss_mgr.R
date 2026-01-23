#' BRFSS_Params R6 Class
#'
#' @export
BRFSS_Params <-
  R6::R6Class(
    classname = "BRFSS_Params",

    private = list(
      p_names = character(0),
      d_names = character(0),

      params = list(),
      dependencies = list(),

      filename_pvt = "brfss_params.rds",
      dir_pvt = "./data/params/",

      resolve_filename = function(filename, create_dir = FALSE) {

        if(is.null(filename)) {
          filename <- file.path(private$dir_pvt,private$filename_pvt)
          if(is.null(filename)) return(NULL)

          if(!dir.exists(private$dir_pvt)) dir.create(private$dir_pvt)

          return(filename)
        }

        # --- does it begin with a dot ... replace it with the default project dir
        #      that dir is not exposed in a package without rstudioapi

        if(substring(filename,1,1) == ".") {

          filename <- gsub("^[.]", here::here(), filename)

        }

        dir <- dirname(filename)

        if(dir == ".") dir <- private$dir_pvt
        base <- basename(filename)
        filename <- file.path(dir,base)

        filename <- filename %>%
          gsub("[.]rds$", "", .) %>%
          paste0(.,".rds")


        if(!dir.exists(dir)) {
          if(create_dir) {
            dir.create(dir, recursive = TRUE)
          } else {
            message(paste0("dir: <", dir, "> does not exist, ",
                           "either create it or set create_dir = TRUE"))
            return(NULL)
          }
        }

        return(filename)

      },

      get_p_names = function() {

        private$p_names <-
          purrr::map_chr(private$params, \(param) {
            param$name
          })
      },

      get_d_names = function() {

        private$d_names <-
          purrr::map_chr(private$dependencies, \(dep) {
            dep$name
          })
      },

      get_state = function() {
        list(
          params = private$params,
          dependencies = private$dependencies
        )
      },

      # Loads a list back into the private slots
      load_state = function(state) {
        private$params <- state$params
        private$dependencies <- state$dependencies
        invisible(self)
      }

    ),

    public = list(

      add = function(p) {

        private$params <- append(private$params, p)

        private$get_p_names()

      },

      add_dependency = function(p) {

        private$dependencies <- append(private$dependencies, p)


        private$get_d_names()
      },

      as.list = function() {

        vals <- purrr::map(private$params, \(param) {
          param$value
        })

        names(vals) <- private$p_names

        vals

        deps <- purrr::map(private$dependencies, \(dep) {
          dep$evaluate(vals)
        })

        names(deps) <- private$d_names


        c(vals, deps)

      },

      get = function(name) {

        nm <- rlang::as_string(rlang::ensym(name))

        index <- which(self$names == nm)

        if (!length(index)) {
          stop("Unknown parameter: ", nm, call. = FALSE)
        }

        val <- private$params[[index]]$value
        names(val) <- nm
        val
      },

      set = function(...) {

        quos <- rlang::enquos(...)
        q <- quos[1]

        nm <- names(q)

        val <- rlang::eval_tidy(q[[1]])
        index <- which(self$names == nm)

        if(length(index) == 0) {

          message(paste0("<", nm, "> is not a valid parameter"))
          return(invisible())
        }
        p <- private$params[[index]]


        if(is.null(val) && !is.null(p$on_null)) {
          do.call( p$on_null, list())
        } else if(is.na(val) && !is.null(p$on_na)) {
          do.call( p$on_na, list())
        } else {
          do.call(p$on_change,list(val))
        }
      },

      patterize = function(str_in) {

        pttrns <- self$patterns

      },

      load = function(filename = NULL) {

        filename <- private$resolve_filename(filename)

        if(!is.null(filename)) {

          file_ok <- file.exists(filename)

          if(file_ok) {
            state <- readRDS(filename)
            private$load_state(state)

            private$get_p_names()
            private$get_d_names()
          } else {

            message("file for load does not exist")
          }

        }

        return(!is.null(filename) && file_ok)
      },

      save = function(filename = NULL, create_dir = FALSE) {


        filename <- private$resolve_filename(filename = filename, create_dir)

        state <- private$get_state()

        if(!is.null(filename)) saveRDS(state, file = filename)

        return(!is.null(filename))
      }

    ),

    active = list(

      names = function(value) {

        if(!missing(value)) {
          message("<names> property is read-only")
          return(NULL)
        }

        c(private$p_names, private$d_names)
      },

      filename = function(value) {

        if(!missing(value)) private$filename_pvt <- value

        private$filename_pvt

      },

      patterns = function(value) {

        if(!missing(value)) {
          message("<patterns> property is read-only")
          return(NULL)
        }

        vals <- purrr::map_chr(private$params, \(param) {
          as.character(param$value)

        })

        names(vals) <- purrr::map_chr(private$params, \(param) {
          param$pattern
        })

        deps <- purrr::map_chr(private$dependencies, \(dep) {
          as.character(dep$evaluate(vals))

        })

        names(deps) <- purrr::map_chr(private$dependencies, \(dep) {
          dep$pattern
        })

        c(vals, deps)
      }


    )
  )


BRFSS_Param <-
  R6::R6Class(
    classname = "BRFSS_Param",

    private = list(
      name_pvt = "",
      value_pvt = NULL,
      values_pvt = NULL,
      pattern_pvt = "",

      on_change_pvt = NULL,
      on_null_pvt = NULL,
      on_na_pvt = NULL

    ),

    public = list(

      initialize = function(name = NULL,
                            value = NULL,
                            pattern = "",
                            values = NULL,
                            on_change = NULL,
                            on_null = NULL,
                            on_na = NULL) {


        private$name_pvt = name
        private$values_pvt = values
        private$pattern_pvt = pattern

        private$on_change_pvt = on_change
        private$on_null_pvt = on_null
        private$on_na_pvt = on_na

        if(is.null(value) && !is.null(on_null)) {
          do.call( on_null, list())
        } else if(is.na(value) && !is.null(on_na)) {
          do.call( on_na, list())
        } else {
          do.call(on_change,list(value))
        }


      }


    ),

    active = list(

      value = function(val) {

        if(!missing(val)) {
          private$value_pvt <- val
        }

        return(private$value_pvt)


      },

      values = function(val) {

        if(!missing(val)) {
          private$values_pvt <- val
        }

        return(private$values_pvt)


      },

      name = function(val) {

        if(!missing(val)) {
          private$name_pvt <- val
        }

        return(private$name_pvt)


      },

      pattern = function(val) {

        if(!missing(val)) {
          private$pattern_pvt <- val
        }

        return(private$pattern_pvt)


      },

      on_change = function(f) {

        if(!missing(f)) {
          if(!is.function(f)) {
            message("<f> must be a function")
            return(NULL)
          }
          private$on_change_pvt <-  f
          return(invisible(NULL))
        }

        return(private$on_change_pvt)


      },

      on_null = function(f) {

        if(!missing(f)) {
          if(!is.function(f)) {
            message("<f> must be a function")
            return(NULL)
          }
          private$on_null_pvt <-  f
          return(invisible(NULL))
        }

        return(private$on_null_pvt)


      },

      on_na = function(f) {

        if(!missing(f)) {
          if(!is.function(f)) {
            message("<f> must be a function")
            return(NULL)
          }
          private$on_na_pvt <-  f
          return(invisible(NULL))
        }

        return(private$on_na_pvt)


      }

    )

  )

BRFSS_DependentParam <-
  R6::R6Class(
    classname = "BRFSS_DependentParam",
    inherit = BRFSS_Param,

    private = list(
      expression_pvt = ""

    ),

    public = list(

      initialize = function(expression) {

        if(!inherits(expression, "expression")) {
          expression <- as.character(expression)
          expression <- rlang::parse_expr(expression)
        }
        private$expression_pvt <-  expression
      },

      evaluate = function(vars) {

        rlang::eval_tidy(private$expression_pvt, vars)
      }
    )
  )

Yr_Param <-
  R6::R6Class(
    classname = "Yr_Param",
    inherit = BRFSS_DependentParam,

    public = list(

      initialize = function() {

        super$initialize(expression = "year%%100")
        private$pattern_pvt  <-  "YR"
        private$name_pvt <- "yr"
      }
    )
  )

Year_Param <- R6::R6Class(
  classname = "Year_Param",
  inherit = BRFSS_Param,

  public = list(

    initialize = function(year) {


      on_change <- function(year) {

        year <- tryCatch({
          as.integer(year)
        }, warning = function(w) {
          message("bad argument for <year>, setting default: last year")
          lubridate::year(Sys.Date()) - 1
        }, error = function(e) {
          message("bad argument for <year>, setting default: last year")
          lubridate::year(Sys.Date()) - 1
        })

        private$value_pvt <- year
      }

      on_null <- function() {
        private$value_pvt <- lubridate::year(Sys.Date()) - 1
      }

      on_na <- function() {
        private$value_pvt <- lubridate::year(Sys.Date()) - 1
      }

      if(missing(year)) year <- lubridate::year(Sys.Date()) - 1

      super$initialize(name = "year",
                       value = year,
                       pattern =  "YEAR",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)

    }
  )
)

Extent_Param <- R6::R6Class(
  classname = "Extent_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(extent) {

      on_change <- function(extent) {

        extent <- tolower(extent)

        if(!extent %in% private$values_pvt) {
          warning(paste0("extent must be one of <",
                         paste0(private$values_pvt, collapse = ", "),
                         ">"))
        }

        private$value_pvt <- extent
      }

      on_null <- function() {
        private$value_pvt <- private$values_pvt[1]
      }

      on_na <- function() {
        private$value_pvt <- private$values_pvt[1]
      }

      if(missing(extent)) extent <- "local"

      super$initialize(name = "extent",
                       value = extent,
                       values = c("local","public", "monthly"),
                       pattern =  "EXT",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

Source_Param <- R6::R6Class(
  classname = "Source_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(source) {

      on_change <- function(source) {

        source <- tolower(source)

        if(!source %in% private$values_pvt) {
          warning(paste0("source must be one of <",
                         paste0(private$values_pvt, collapse = ", "), ">"))
        }

        private$value_pvt <- source

      }

      on_null <- function() {
        private$value_pvt <- private$values_pvt[1]
      }

      on_na <- function() {
        private$value_pvt <- private$values_pvt[1]
      }

      if(missing(source)) source <- "ascii"

      super$initialize(name = "source",
                       value = source,
                       values = c("ascii","sas"),
                       pattern =  "SRC",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

Weight_Param <- R6::R6Class(
  classname = "Weight_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(weight) {

      on_change <- function(weight) {

        if(!inherits(weight, "character")) {
          warning(paste0("weight must class character"))
        }

        private$value_pvt <- weight

      }

      on_null <- function() {
        private$value_pvt <- "_LLCPWT"
      }

      on_na <- function() {
        private$value_pvt <- NA
      }

      if(missing(weight)) weight <-  "_LLCPWT"

      super$initialize(name = "weight",
                       value = weight,
                       values = NULL,
                       pattern =  "WT",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

GeogFlag_Param <- R6::R6Class(
  classname = "GeogFlag_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(geog_flag) {

      on_change <- function(geog_flag) {

        if(is.logical(geog_flag)) {

          geog_flag <- if(geog_flag) geog_flag <- "on" else geog_flag <- "off"

        } else if(is.character(geog_flag)) {

          if(!geog_flag %in% c("on", "off")) {
            message("bad argument for <geog_flag>, setting default: on")
            geog_flag <- "on"
          }
        } else {
          message("bad argument for <geog_flag>, setting default: on")
          geog_flag <- "on"

        }
        private$value_pvt <- geog_flag
      }

      on_null <- function() {
        private$value_pvt <- "on"
      }

      on_na <- function() {
        private$value_pvt <- "on"
      }

      if(missing(geog_flag)) geog_flag <- "on"

      super$initialize(name = "geog_flag",
                       value = geog_flag,
                       values = c("on", "off"),
                       pattern =  "GFLAG",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)


Weighting_Param <- R6::R6Class(
  classname = "Weighting_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(weighting) {

      on_change <- function(weighting) {

        if(is.character(weighting)) {
          weighting <- tolower(weighting)

          if(!weighting %in% c("on", "off", "true", "false")) {
            message("bad argument for <weighting>, setting default: TRUE")
            weighting <- TRUE
          } else {
            weighting <- weighting %in% c("on", "true")
          }
        } else if(!is.logical(weighting)) {
          message("bad argument for <weighting>, setting default: TRUE")
          weighting <- TRUE

        }

        private$value_pvt <- weighting
      }

      on_null <- function() {
        private$value_pvt <- TRUE
      }

      on_na <- function() {
        private$value_pvt <- TRUE
      }

      if(missing(weighting)) weighting <- TRUE

      super$initialize(name = "weighting",
                       value = weighting,
                       pattern =  "WTG",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

Version_Param <- R6::R6Class(
  classname = "Version_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(version) {

      on_change <- function(version) {


        version <- tryCatch({
          as.integer(version)
        }, warning = function(w) {
          message("bad argument for <version>, setting default: 0")
          return(0)
        }, error = function(e) {
          message("bad argument for <version>, setting default: 0")
          return(0)
        })

        private$value_pvt <- version
      }

      on_null <- function() {
        private$value_pvt <- 0
      }

      on_na <- function() {
        private$value_pvt <- 0
      }

      if(missing(version)) version <- 0

      super$initialize(name = "version",
                       value = version,
                       values = 0:5,
                       pattern =  "VERS",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)


Month_Param <- R6::R6Class(
  classname = "Month_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(month) {

      on_change <- function(month) {

        if(is.numeric(month)) {
          if(between(month,1, 12)) {
            private$value_pvt <- month
            return(invisible())
          } else {
            message("invalid month")
            return(invisible())
          }
        }

        month_chr <- gsub("[^A-z]","",as.character(month))
        month_int <- gsub("[^0-9]","",as.character(month))

        if(nchar(month_chr) > 2) {

          imonth <- which(toupper(month.abb)  %in% toupper(month))
          if(length(imonth) == 0)  imonth <- which(toupper(month.name)  %in% toupper(month))

          if(length(imonth) == 0) {
            message("invalid month")
            return(invisible())

          }

          private$value_pvt <- imonth
          return(invisible())
        } else if(nchar(month_int) <3) {
          month <- as.integer(month)
          private$value_pvt <-month
          return(invisible())
        }

      }


      on_null <- function() {
        private$value_pvt <- lubridate::month(Sys.Date())
      }

      on_na <- function() {
        private$value_pvt <- lubridate::month(Sys.Date())
      }

      if(missing(month)) month <- lubridate::month(Sys.Date())

      super$initialize(name = "month",
                       value = month,
                       values = 1:12,
                       pattern =  "MONTH",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

YTD_Param <- R6::R6Class(
  classname = "YTD_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(ytd) {

      on_change <- function(ytd) {

        if(is.logical(ytd)) {

          ytd <- if(ytd) ytd <- "on" else ytd <- "off"

        } else if(is.character(ytd)) {

          if(!ytd %in% c("on", "off")) {
            message("bad argument for <ytd>, setting default: on")
            ytd <- "off"
          }
        } else {
          message("bad argument for <ytd>, setting default: on")
          ytd <- "off"

        }
        private$value_pvt <- ytd
      }

      on_null <- function() {
        private$value_pvt <- "off"
      }

      on_na <- function() {
        private$value_pvt <- "off"
      }

      if(missing(ytd)) ytd <- "on"

      super$initialize(name = "ytd",
                       value = ytd,
                       values = c("on", "off"),
                       pattern =  "YTD",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

Phone_Param <- R6::R6Class(
  classname = "Phone_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(phone) {

      on_change <- function(phone) {

        if(is.character(phone)) {

          if(!phone %in% c("cell","land", "comb")) {
            message("bad argument for <phone>, setting default: on")
            phone <- "comb"
          }
        } else {
          message("bad argument for <phone>, setting default: on")
          phone <- "comb"

        }
        private$value_pvt <- phone
      }

      on_null <- function() {
        private$value_pvt <- "comb"
      }

      on_na <- function() {
        private$value_pvt <- "comb"
      }

      if(missing(phone)) phone <- "comb"

      super$initialize(name = "phone",
                       value = phone,
                       values = c("cell","land", "comb"),
                       pattern =  "PHON",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)

Geog_Param <- R6::R6Class(
  classname = "Geog_Param",
  inherit = BRFSS_Param,


  public = list(

    initialize = function(geog) {

      on_change <- function(geog) {

        geog_chr <- gsub("[^A-z]","",as.character(geog))
        geog_int <- gsub("[^0-9]","",as.character(geog))

        if(nchar(geog_int) == 2) geog <- as.integer(geog)

        if(is.numeric(geog)) {
          geog <- get_geogs_all() %>% filter(Id == geog) %>% pull(Abbrev)

        } else {
          if(nchar(geog) != 2) {
            geog <- get_geogs_all() %>% filter(Geog == geog) %>% pull(Abbrev)
          }
        }

        geog <- toupper(geog)

        private$value_pvt <-geog
      }

      on_null <- function() {
        private$value_pvt <- ""
      }

      on_na <- function() {
        private$value_pvt <- ""
      }

      if(missing(geog)) geog <- my_geog()

      super$initialize(name = "geog",
                       value = geog,
                       values = NULL,
                       pattern =  "GEOG",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  )
)


#'
#'   ###################################################
#'   ##
#'   ##    phone
#'
#'   my_brfss$phone$value <- "cell"
#'   my_brfss$phone$values <- c("cell","land", "comb")
#'   my_brfss$phone$pattern <- "PHON"
#'
#'

#' BRFSS_Params DataR6 Class
#'
#' @export
BRFSS_DataParams <-
  R6::R6Class(
    classname = "BRFSS_DataParams",
    inherit = BRFSS_Params,

    public = list(
      initialize = function(year = NULL, geog = NULL, extent = NULL, source = NULL, version = 0,
                            weight = NULL, weighting = NULL) {
        super$add(Year_Param$new(year = year))
        super$add(Extent_Param$new(extent = extent))
        super$add(Source_Param$new(source = source))
        super$add(Version_Param$new(version = version))
        super$add(Weight_Param$new(weight = weight))
        super$add(Weighting_Param$new(weighting = weighting))
        super$add(Geog_Param$new(geog = geog))
        super$add(GeogFlag_Param$new())
        super$add_dependency(Yr_Param$new())

      }
    )
  )

#' BRFSS_Params DataR6 Class
#'
#' @export
BRFSS_LocalDataParams <-
  R6::R6Class(
    classname = "BRFSS_LocalDataParams",
    inherit = BRFSS_DataParams,

    public = list(
      initialize = function(year = NULL) {
        super$initialize(year = year, extent = "local", source = "ascii", geog = "VI")


      }
    )

  )

#' BRFSS_Params DataR6 Class
#'
#' @export
BRFSS_PublicDataParams <-
  R6::R6Class(
    classname = "BRFSS_PublicDataParams",
    inherit = BRFSS_DataParams,

    public = list(

      initialize = function(year = NULL, geog = NULL, source = "sas") {
        super$initialize(year = year, extent = "public", source = source, geog = geog)

      }
    )

  )

#' BRFSS_Params Data R6 Class
#'
#' @export
BRFSS_Monthly_Params <-
  R6::R6Class(
    classname = "BRFSS_Monthly_Params",
    inherit = BRFSS_Params,

    public = list(
      initialize = function() {
        super$add(Year_Param$new())
        super$add(Geog_Param$new())
        super$add(Month_Param$new())
        super$add(YTD_Param$new())
        super$add(Phone_Param$new())

      }
    )
  )


