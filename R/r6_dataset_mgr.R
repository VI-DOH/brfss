#' @title DatasetParams
#' @description A class for a collection of parameters for interacting with BRFSS data.
#' @family BRFSS Dataset
#' #' @export
DataSetParams <-
  R6::R6Class(
    classname = "DataSetParams",

    private = list(

      # ------  properties

      ..p_names = character(0),
      ..d_names = character(0),

      ..params = list(),
      ..dependencies = list(),

      ..filename = "recent.rds",
      ..dir = "./data/datasets/",

      # ------  methods

      ..resolve_filename = function(filename, create_dir = FALSE) {

        if(is.null(filename)) {
          filename <- file.path(private$..dir,private$..filename)
          if(is.null(filename)) return(NULL)

          if(!dir.exists(private$..dir)) dir.create(private$..dir)

          return(filename)
        }

        # --- does it begin with a dot ... replace it with the default project dir
        #

        if(substring(filename,1,1) == ".") {

          filename <- gsub("^[.]", here::here(), filename)

        }

        dir <- dirname(filename)

        if(dir == ".") dir <- private$..dir
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

      ..get_p_names = function() {

        private$..p_names <-
          purrr::map_chr(private$..params, \(param) {
            param$name
          })
      },

      ..get_d_names = function() {

        private$..d_names <-
          purrr::map_chr(private$..dependencies, \(dep) {
            dep$name
          })
      },

      ..get_state = function() {

        list(
          params = private$..params,
          dependencies = private$..dependencies
        )
      },

      # Loads a list back into the private slots
      ..load_state = function(state) {
        private$..params <- state$params
        private$..dependencies <- state$dependencies
        invisible(self)
      }

    ),

    public = list(

      initialize = function(filename = NULL, ...) {

        tryCatch(

          expr =  {

            test <- self$read(filename = filename)

            self$set(...)
            #self$set_legacy()  # remove when all conversions to R6 are complete
          },

          error = function(e) {

          },

          warning = function(w) {


          }
        )
      },

      print = function() {

        x <- self$as.vector()
        class(x) <- "character"

        print(x)
      },

      load = function(from) {

        if(!inherits(from, "brfss_dataset")) return(FALSE)

        purrr::iwalk(from, \(x, nm) {

          index <- which(private$..p_names == nm)
          if(length(index) > 0) private$..params[[index]]$set(x)

        })
        self$set_legacy()

      },

      add = function(p) {

        private$..params[[p$name]] <- p

        private$..get_p_names()

      },

      add_dependency = function(p) {

        private$..dependencies <- append(private$..dependencies, p)


        private$..get_d_names()
      },

      as.vector = function() {

        x <- self$as.list() %>% unlist()

        if(is.null(x)) return(NULL)
        structure(
          x,
          class = c(class(x),"brfss_dataset")
        )
      },

      as.list = function() {

        vals <- purrr::map(private$..params, \(param) {
          param$value
        })

        names(vals) <- private$..p_names


        deps <- purrr::map(private$..dependencies, \(dep) {
          dep$evaluate(vals)
        })

        names(deps) <- private$..d_names

        x <- c(vals, deps)

        structure(
          x,
          class = c(class(x),"brfss_dataset")
        )

      },

      restore = function(value) {

        if(missing(value) || !inherits(value, "environment")) {
          message("list must be an <environment> object")
          return(FALSE)
        }

        private <- value
        return(TRUE)

      },

      get = function(name) {

        nm <- rlang::as_string(rlang::ensym(name))

        index <- which(self$names == nm)

        if (!length(index)) {
          stop("Unknown parameter: ", nm, call. = FALSE)
        }

        val <- private$..params[[index]]$value
        names(val) <- nm
        val
      },

      set = function(...) {

        quos <- rlang::enquos(...)
        names <- names(quos)

        purrr::iwalk(quos, \(q, nm) {

          val <- rlang::eval_tidy(q)
          index <- which(self$names == nm)

          if(length(index) == 0) {

            message(paste0("<", nm, "> is not a valid parameter"))
            return(invisible())
          }
          p <- private$..params[[index]]

          p$set(val)
        })

        self$save()

      },

      read = function(filename = NULL, update = TRUE) {

        filename <- private$..resolve_filename(filename)

        if(!is.null(filename)) {

          file_ok <- file.exists(filename)

          if(file_ok) {
            state <- readRDS(filename)
          } else {

            message("file for load does not exist")
            return(NULL)
          }

          if(update) {
            private$..load_state(state)

            private$..get_p_names()
            private$..get_d_names()

            return(TRUE)

          } else {
            return(state)
          }

        }

        return(!is.null(filename) && file_ok)
      },

      save = function(filename = NULL, create_dir = FALSE) {


        filename <- private$..resolve_filename(filename = filename, create_dir)

        state <- private$..get_state()

        if(!is.null(filename)) saveRDS(state, file = filename)

        return(!is.null(filename))
      },

      set_legacy = function() {

        brfss::brfss.params(self$as.list())
      }

    ),

    active = list(

      names = function(value) {

        if(!missing(value)) {
          message("<names> property is read-only")
          return(NULL)
        }

        c(private$..p_names, private$..d_names)
      },

      filename = function(value) {

        if(!missing(value)) private$..filename <- value

        private$..filename

      },

      patterns = function(value) {

        if(!missing(value)) {
          message("<patterns> property is read-only")
          return(NULL)
        }

        x <- self$as.vector()

        vnames <- purrr::map_chr(private$..params, \(param) {
          param$pattern
        })

        dnames <- purrr::map_chr(private$..dependencies, \(dep) {
          dep$pattern
        })

        names(x) <- c(vnames, dnames)

        return(x)

      },

      snapshot = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return()
        }

        return(private)

      }

    )
  )

#' @title Dataset_Param
#' @description A class for a single parameter for interacting with BRFSS data.
#'   Examples of parameters are year, source, extent, and geog
#' @family BRFSS Dataset
Dataset_Param <-
  R6::R6Class(
    classname = "Dataset_Param",

    private = list(
      ..name = "",
      ..value = NULL,
      ..values = NULL,
      ..pattern = "",

      on_change = function(value) {
        private$..value <- value
      },

      on_null  = function() {
        private$..value <- NULL
      },

      on_na = function() {
        private$..value <- NA
      }




    ),

    public = list(

      initialize = function(name = NULL,
                            value = NULL,
                            pattern = "",
                            values = NULL) {



        private$..name = name
        private$..values = values
        private$..pattern = pattern

        #
        self$set(value)
      },

      set = function(value) {

        if(is.null(value)) {
          private$on_null()
        } else if(is.na(value)) {
          private$on_na()
        } else {
          private$on_change(value)
        }

      }


    ),

    active = list(

      value = function(val) {

        if(!missing(val)) {
          private$..value <- val
        }

        return(private$..value)


      },

      values = function(val) {

        if(!missing(val)) {
          private$..values <- val
        }

        return(private$..values)


      },

      name = function(val) {

        if(!missing(val)) {
          private$..name <- val
        }

        return(private$..name)


      },

      pattern = function(val) {

        if(!missing(val)) {
          private$..pattern <- val
        }

        return(private$..pattern)


      }

    )

  )

#' @title BRFSS_DependentParam
#' @description A class for a single parameter dependent on other paranters for interacting with BRFSS data.
#'   Examples of a dependent parameter is yr which is a two-digit represntation of parameter year
#' @family BRFSS Dataset
#'
BRFSS_DependentParam <-
  R6::R6Class(
    classname = "BRFSS_DependentParam",
    inherit = Dataset_Param,

    private = list(
      ..expression = ""

    ),

    public = list(

      initialize = function(expression) {

        if(!inherits(expression, "expression")) {
          expression <- as.character(expression)
          expression <- rlang::parse_expr(expression)
        }
        private$..expression <-  expression
      },

      evaluate = function(vars) {

        rlang::eval_tidy(private$..expression, vars)
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
        private$..pattern  <-  "YR"
        private$..name <- "yr"
      }
    )
  )

Year_Param <- R6::R6Class(
  classname = "Year_Param",
  inherit = Dataset_Param,

  public = list(

    initialize = function(year) {

      if(missing(year)) year <- lubridate::year(Sys.Date()) - 1

      super$initialize(name = "year",
                       value = year,
                       pattern =  "YEAR"
      )
    }
  ),

  private = list(

    on_change = function(year) {

      year <- tryCatch({
        as.integer(year)
      }, warning = function(w) {
        message("bad argument for <year>, setting default: last year")
        lubridate::year(Sys.Date()) - 1
      }, error = function(e) {
        message("bad argument for <year>, setting default: last year")
        lubridate::year(Sys.Date()) - 1
      })

      private$..value <- year
    },

    on_null = function() {
      private$..value <- lubridate::year(Sys.Date()) - 1
    },

    on_na = function() {
      private$..value <- lubridate::year(Sys.Date()) - 1
    }
  )

)


Extent_Param <- R6::R6Class(
  classname = "Extent_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(extent) {


      if(missing(extent)) extent <- "local"

      super$initialize(name = "extent",
                       value = extent,
                       values = c("local","public", "monthly"),
                       pattern =  "EXT")


    }
  ),

  private = list(

    on_change = function(extent) {

      extent <- tolower(extent)

      if(!extent %in% private$..values) {
        warning(paste0("extent must be one of <",
                       paste0(private$..values, collapse = ", "),
                       ">"))
        message("Setting extent to ",  private$..values[1])
        private$..value <- private$..values[1]

      } else {

        private$..value <- extent

      }
    },

    on_null = function() {
      private$..value <- private$..values[1]
    },

    on_na = function() {
      private$..value <- private$..values[1]
    }

  )
)

Source_Param <- R6::R6Class(
  classname = "Source_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(source) {



      if(missing(source)) source <- "ascii"

      super$initialize(name = "source",
                       value = source,
                       values = c("ascii","sas"),
                       pattern =  "SRC")


    }
  ),

  private = list(

    on_change = function(source) {

      source <- tolower(source)

      if(!source %in% private$..values) {
        warning(paste0("source must be one of <",
                       paste0(private$..values, collapse = ", "), ">"))
        message("Setting source to ",  private$..values[1])
        private$..value <- private$..values[1]
      } else {

        private$..value <- source
      }

    },

    on_null = function() {
      private$..value <- private$..values[1]
    },

    on_na = function() {
      private$..value <- private$..values[1]
    }
  )
)

Weight_Param <- R6::R6Class(
  classname = "Weight_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(weight) {



      if(missing(weight)) weight <-  "_LLCPWT"

      super$initialize(name = "weight",
                       value = weight,
                       values = NULL,
                       pattern =  "WT")


    }
  ),

  private = list(

    on_change = function(weight) {

      if(!inherits(weight, "character")) {
        warning(paste0("weight must class character"))
      }

      private$..value <- weight

    },

    on_null = function() {
      private$..value <- "_LLCPWT"
    },

    on_na = function() {
      private$..value <- NA
    }
  )
)

GeogFlag_Param <- R6::R6Class(
  classname = "GeogFlag_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(geog_flag) {

      if(missing(geog_flag)) geog_flag <- "on"

      super$initialize(name = "geog_flag",
                       value = geog_flag,
                       values = c("on", "off"),
                       pattern =  "GFLAG")


    }
  ),

  private = list(

    on_change = function(geog_flag) {

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
      private$..value <- geog_flag
    },

    on_null = function() {
      private$..value <- "on"
    },

    on_na = function() {
      private$..value <- "on"
    }

  )
)


Weighting_Param <- R6::R6Class(
  classname = "Weighting_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(weighting) {



      if(missing(weighting)) weighting <- TRUE

      super$initialize(name = "weighting",
                       value = weighting,
                       pattern =  "WTG")


    }
  ),

  private = list(

    on_change = function(weighting) {

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

      private$..value <- weighting
    },

    on_null = function() {
      private$..value <- TRUE
    },

    on_na = function() {
      private$..value <- TRUE
    }
  )
)

Version_Param <- R6::R6Class(
  classname = "Version_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(version) {



      if(missing(version)) version <- 0

      super$initialize(name = "version",
                       value = version,
                       values = 0:5,
                       pattern =  "VERS")


    }
  ),

  private = list(

    on_change = function(version) {


      version <- tryCatch({
        as.integer(version)
      }, warning = function(w) {
        message("bad argument for <version>, setting default: 0")
        return(0)
      }, error = function(e) {
        message("bad argument for <version>, setting default: 0")
        return(0)
      })

      private$..value <- version
    },

    on_null = function() {
      private$..value <- 0
    },

    on_na = function() {
      private$..value <- 0
    }

  )
)


Month_Param <- R6::R6Class(
  classname = "Month_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(month) {



      if(missing(month)) month <- lubridate::month(Sys.Date())

      super$initialize(name = "month",
                       value = month,
                       values = 1:12,
                       pattern =  "MONTH")


    }
  ),

  private = list(

    on_change = function(month) {

      if(is.numeric(month)) {
        if(between(month,1, 12)) {
          private$..value <- month
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

        private$..value <- imonth
        return(invisible())
      } else if(nchar(month_int) <3) {
        month <- as.integer(month)
        private$..value <-month
        return(invisible())
      }

    },


    on_null = function() {
      private$..value <- lubridate::month(Sys.Date())
    },

    on_na = function() {
      private$..value <- lubridate::month(Sys.Date())
    }

  )
)

YTD_Param <- R6::R6Class(
  classname = "YTD_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(ytd) {

      if(missing(ytd)) ytd <- "on"

      super$initialize(name = "ytd",
                       value = ytd,
                       values = c("on", "off"),
                       pattern =  "YTD")

    }
  ),

  private = list(

    on_change = function(ytd) {

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
      private$..value <- ytd
    },

    on_null = function() {
      private$..value <- "off"
    },

    on_na = function() {
      private$..value <- "off"
    }
  )
)

Phone_Param <- R6::R6Class(
  classname = "Phone_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(phone) {



      if(missing(phone)) phone <- "comb"

      super$initialize(name = "phone",
                       value = phone,
                       values = c("cell","land", "comb"),
                       pattern =  "PHON",
                       on_change = on_change,
                       on_null = on_null,
                       on_na = on_na)


    }
  ),

  private = list(

    on_change = function(phone) {

      if(is.character(phone)) {

        if(!phone %in% c("cell","land", "comb")) {
          message("bad argument for <phone>, setting default: comb")
          phone <- "comb"
        }
      } else {
        message("bad argument for <phone>, setting default: comb")
        phone <- "comb"

      }
      private$..value <- phone
    },

    on_null = function() {
      private$..value <- "comb"
    },

    on_na = function() {
      private$..value <- "comb"
    }

  )
)

Geog_Param <- R6::R6Class(
  classname = "Geog_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(geog) {

      if(missing(geog) || is.null(geog) || geog == "") {

        private$geog_mgr <- GeogMgr$new()

        geog <-  private$geog_mgr$abbrev
      }

      super$initialize(name = "geog",
                       value = geog,
                       values = NULL,
                       pattern =  "GEOG")


    }
  ),

  private = list(

    geog_mgr = NULL,

    on_change = function(geog) {

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

      private$..value <-geog
    },

    on_null = function() {
      private$..value <- ""
    },

    on_na = function() {
      private$..value <- ""
    }

  )
)


#' DataSetMgr DataR6 Class
#'
#' @family BRFSS Tools
#' @family BRFSS Dataset
#' @export
DataSetMgr <-
  R6::R6Class(
    classname = "DataSetMgr",
    inherit = DataSetParams,

    private = list(

      merge_current = function(...) {

        new_vals <- list(...)
        old_vals <- self$read(update = FALSE)

        if(is.null(old_vals)) {

          year <- new_vals$year
          geog <- new_vals$geog
          geog_flag <- new_vals$geog_flag
          extent <- new_vals$extent
          source <- new_vals$source
          version <- 0
          weight <- new_vals$weight
          weighting <- new_vals$weighting

        } else {

          year <- new_vals$year %||% old_vals$params[["year"]]$value
          geog <- new_vals$geog %||% old_vals$params[["geog"]]$value
          geog_flag <- new_vals$geog_flag %||% old_vals$params[["geog_flag"]]$value
          extent <- new_vals$extent %||% old_vals$params[["extent"]]$value
          source <- new_vals$source %||% old_vals$params[["source"]]$value
          version <- 0
          weight <- new_vals$weight %||% old_vals$params[["weight"]]$value
          weighting <- new_vals$weighting %||% old_vals$params[["weighting"]]$value

        }

        super$add(Year_Param$new(year = year))
        super$add(Extent_Param$new(extent = extent))
        super$add(Source_Param$new(source = source))
        super$add(Version_Param$new(version = version))
        super$add(Weight_Param$new(weight = weight))
        super$add(Weighting_Param$new(weighting = weighting))
        super$add(Geog_Param$new(geog = geog))
        super$add(GeogFlag_Param$new(geog_flag = geog_flag))
        super$add_dependency(Yr_Param$new())

        super$save()

      }
    ),

    public = list(
      initialize = function(year = NULL, geog = NULL, extent = NULL,
                            source = NULL, geog_flag = NULL, version = 0,
                            weight = NULL, weighting = NULL) {


        private$merge_current(year = year, geog = geog, extent = extent,
                              source  = source, geog_flag = geog_flag, version = version,
                              weight = weight, weighting = weighting)


      }
    )
  )

#' @export
DataSetMgr$current <- function() {

  ds <- DataSetMgr$new()
  ds$as.vector()


}

#' DataSetMgr DataR6 Class
#' @family BRFSS Tools
#' @family BRFSS Dataset
#'
#' @export
LocalDataSetMgr <-
  R6::R6Class(
    classname = "LocalDataSetMgr",
    inherit = DataSetMgr,

    public = list(
      initialize = function(year = NULL, version = 0, geog = NULL, source = NULL) {

        if(is.null(geog)) {
          gm <- GeogMgr$new()
          geog <- gm$abbrev
        }

        source <- source %||% "ascii"

        super$initialize(year = year, extent = "local", source = "ascii",
                         geog = geog, version = version)


      }
    )

  )

#' DataSetMgr DataR6 Class
#' @family BRFSS Tools
#' @family BRFSS Dataset
#'
#' @export
PublicDataSetMgr <-
  R6::R6Class(
    classname = "PublicDataSetMgr",
    inherit = DataSetMgr,

    public = list(

      initialize = function(year = NULL, geog = NULL, source = "sas",
                            geog_flag = NULL, version = 0) {

        super$initialize(year = year, extent = "public", source = source,
                         geog_flag = geog_flag, version = version, geog = geog)

      }
    )

  )

#' DataSetMgr Data R6 Class
#'
#' @export
MonthlyDataSetMgr <-
  R6::R6Class(
    classname = "MonthlyDataSetMgr",
    inherit = DataSetMgr,

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


