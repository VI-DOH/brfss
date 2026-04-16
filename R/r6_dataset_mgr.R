#' DataSetMgr R6 Class
#'
#' @export
DataSetParams <-
  R6::R6Class(
    classname = "DataSetParams",

    private = list(
      p_names = character(0),
      d_names = character(0),

      params = list(),
      dependencies = list(),

      filename_pvt = "recent.rds",
      dir_pvt = "./data/datasets/",

      resolve_filename = function(filename, create_dir = FALSE) {

        if(is.null(filename)) {
          filename <- file.path(private$dir_pvt,private$filename_pvt)
          if(is.null(filename)) return(NULL)

          if(!dir.exists(private$dir_pvt)) dir.create(private$dir_pvt)

          return(filename)
        }

        # --- does it begin with a dot ... replace it with the default project dir
        #

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

      initialize = function(filename = NULL, ...) {

        tryCatch(

          expr =  {
            self$read(filename = filename)
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

          index <- which(private$p_names == nm)
          if(length(index) > 0) private$params[[index]]$set(x)

        })
        self$set_legacy()

      },

      add = function(p) {

        private$params[[p$name]] <- p

        private$get_p_names()

      },

      add_dependency = function(p) {

        private$dependencies <- append(private$dependencies, p)


        private$get_d_names()
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

        vals <- purrr::map(private$params, \(param) {
          param$value
        })

        names(vals) <- private$p_names


        deps <- purrr::map(private$dependencies, \(dep) {
          dep$evaluate(vals)
        })

        names(deps) <- private$d_names

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

        val <- private$params[[index]]$value
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
          p <- private$params[[index]]

          p$set(val)
        })

        self$save()

      },

      read = function(filename = NULL, update = TRUE) {

        filename <- private$resolve_filename(filename)

        if(!is.null(filename)) {

          file_ok <- file.exists(filename)

          if(file_ok) {
            state <- readRDS(filename)
          } else {

            message("file for load does not exist")
            return()
          }

          if(update) {
            private$load_state(state)

            private$get_p_names()
            private$get_d_names()

          } else {
            return(state)
          }

        }

        return(!is.null(filename) && file_ok)
      },

      save = function(filename = NULL, create_dir = FALSE) {


        filename <- private$resolve_filename(filename = filename, create_dir)

        state <- private$get_state()

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

        x <- self$as.vector()

        vnames <- purrr::map_chr(private$params, \(param) {
          param$pattern
        })

        dnames <- purrr::map_chr(private$dependencies, \(dep) {
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


Dataset_Param <-
  R6::R6Class(
    classname = "Dataset_Param",

    private = list(
      name_pvt = "",
      value_pvt = NULL,
      values_pvt = NULL,
      pattern_pvt = "",

      on_change = function(value) {
        private$value_pvt <- value
      },

      on_null  = function() {
        private$value_pvt <- NULL
      },

      on_na = function() {
        private$value_pvt <- NA
      }




    ),

    public = list(

      initialize = function(name = NULL,
                            value = NULL,
                            pattern = "",
                            values = NULL) {



        private$name_pvt = name
        private$values_pvt = values
        private$pattern_pvt = pattern

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


      }

    )

  )

BRFSS_DependentParam <-
  R6::R6Class(
    classname = "BRFSS_DependentParam",
    inherit = Dataset_Param,

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

      private$value_pvt <- year
    },

    on_null = function() {
      private$value_pvt <- lubridate::year(Sys.Date()) - 1
    },

    on_na = function() {
      private$value_pvt <- lubridate::year(Sys.Date()) - 1
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

      if(!extent %in% private$values_pvt) {
        warning(paste0("extent must be one of <",
                       paste0(private$values_pvt, collapse = ", "),
                       ">"))
        message("Setting extent to ",  private$values_pvt[1])
        private$value_pvt <- private$values_pvt[1]

      } else {

        private$value_pvt <- extent

      }
    },

    on_null = function() {
      private$value_pvt <- private$values_pvt[1]
    },

    on_na = function() {
      private$value_pvt <- private$values_pvt[1]
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

      if(!source %in% private$values_pvt) {
        warning(paste0("source must be one of <",
                       paste0(private$values_pvt, collapse = ", "), ">"))
        message("Setting source to ",  private$values_pvt[1])
        private$value_pvt <- private$values_pvt[1]
      } else {

        private$value_pvt <- source
      }

    },

    on_null = function() {
      private$value_pvt <- private$values_pvt[1]
    },

    on_na = function() {
      private$value_pvt <- private$values_pvt[1]
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

      private$value_pvt <- weight

    },

    on_null = function() {
      private$value_pvt <- "_LLCPWT"
    },

    on_na = function() {
      private$value_pvt <- NA
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
      private$value_pvt <- geog_flag
    },

    on_null = function() {
      private$value_pvt <- "on"
    },

    on_na = function() {
      private$value_pvt <- "on"
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

      private$value_pvt <- weighting
    },

    on_null = function() {
      private$value_pvt <- TRUE
    },

    on_na = function() {
      private$value_pvt <- TRUE
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

      private$value_pvt <- version
    },

    on_null = function() {
      private$value_pvt <- 0
    },

    on_na = function() {
      private$value_pvt <- 0
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

    },


    on_null = function() {
      private$value_pvt <- lubridate::month(Sys.Date())
    },

    on_na = function() {
      private$value_pvt <- lubridate::month(Sys.Date())
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
      private$value_pvt <- ytd
    },

    on_null = function() {
      private$value_pvt <- "off"
    },

    on_na = function() {
      private$value_pvt <- "off"
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
          message("bad argument for <phone>, setting default: on")
          phone <- "comb"
        }
      } else {
        message("bad argument for <phone>, setting default: on")
        phone <- "comb"

      }
      private$value_pvt <- phone
    },

    on_null = function() {
      private$value_pvt <- "comb"
    },

    on_na = function() {
      private$value_pvt <- "comb"
    }

  )
)

Geog_Param <- R6::R6Class(
  classname = "Geog_Param",
  inherit = Dataset_Param,


  public = list(

    initialize = function(geog) {

      private$geog_mgr <- GeogMgr$new()

      if(missing(geog) || is.null(geog) || geog == "") geog <-  private$geog_mgr$abbrev

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

      private$value_pvt <-geog
    },

    on_null = function() {
      private$value_pvt <- ""
    },

    on_na = function() {
      private$value_pvt <- ""
    }

  )
)


#' DataSetMgr DataR6 Class
#'
#' @export
DataSetMgr <-
  R6::R6Class(
    classname = "DataSetMgr",
    inherit = DataSetParams,

    public = list(
      initialize = function(year = NULL, geog = NULL, extent = NULL,
                            source = NULL, version = 0,
                            weight = NULL, weighting = NULL) {




        x <- self$read(update = FALSE)

        year = year %||% x$params[["year"]]$value
        geog = geog %||% x$params[["geog"]]$value
        extent = extent %||% x$params[["extent"]]$value
        source = source %||% x$params[["source"]]$value
        version = 0
        weight = weight %||% x$params[["weight"]]$value
        weighting = weighting %||% x$params[["weighting"]]$value

        super$add(Year_Param$new(year = year))
        super$add(Extent_Param$new(extent = extent))
        super$add(Source_Param$new(source = source))
        super$add(Version_Param$new(version = version))
        super$add(Weight_Param$new(weight = weight))
        super$add(Weighting_Param$new(weighting = weighting))
        super$add(Geog_Param$new(geog = geog))
        super$add(GeogFlag_Param$new())
        super$add_dependency(Yr_Param$new())

        super$save()
      }
    )
  )

#' DataSetMgr DataR6 Class
#'
#' @export
LocalDataSetMgr <-
  R6::R6Class(
    classname = "LocalDataSetMgr",
    inherit = DataSetMgr,

    public = list(
      initialize = function(year = NULL, version = 0, geog = NULL) {

        if(is.null(geog)) {
          gm <- GeogMgr$new()
          geog <- gm$abbrev
        }

        super$initialize(year = year, extent = "local", source = "ascii",
                         geog = geog, version = version)


      }
    )

  )

#' DataSetMgr DataR6 Class
#'
#' @export
PublicDataSetMgr <-
  R6::R6Class(
    classname = "PublicDataSetMgr",
    inherit = DataSetMgr,

    public = list(

      initialize = function(year = NULL, geog = NULL, source = "sas", version = 0) {

        super$initialize(year = year, extent = "public", source = source,
                         version = version, geog = geog)

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


