
#' Data Manager Class
#'
#' @description
#' Core manager responsible for handling, searching, extracting, and profiling raw and prepped BRFSS datasets.
#' Coordinates structural layouts and files using internal `DataSetMgr` and `FileMgr` instances.
#'
#' @export
DataMgr <-
  R6::R6Class(
    classname = "DataMgr",

    private = list(

      # ------  properties  -------

      ..layout_mgr = NULL,
      ..dataset_mgr = NULL,
      ..file_mgr = NULL,

      # ------  methods  -------

      ..data_path = function(rw = c("r","w")) {

        p <- private

        read <- rw == 'r'
        write <- rw == 'w'

        params <- p$..dataset_mgr$patterns

        fldr <- p$..file_mgr$apply("annual_data_folder")

        if(!dir.exists(fldr) && write) {
          dir.create(fldr, recursive = TRUE)
        }

        file <- p$..file_mgr$apply("annual_data_file")

        path <- paste0(fldr,file)

        if(read && !file.exists(path)) path <- NULL

        path

      },

      ..reweight = function(df, ..., trim = TRUE) {

        df_tots <- df %>% count(...) %>% mutate(pct = n / sum(n))

        df <- df %>%
          left_join(df_tots, by = quo_var(...)) %>%
          mutate(FINAL_WT = `_LLCPWT` * pct)

        if(trim) {
          df <- df %>%
            select(-n, -pct, -`_LLCPWT`)
#          select(Sex, Race, `_STSTR`, FINAL_WT)
        }

        df
      }

    ),

    public = list(

      initialize = function(dataset_mgr = NULL, file_mgr = NULL, ...) {

        if(!is.null(dataset_mgr)) {
          if(inherits(dataset_mgr, "DataSetMgr")) {
            private$..dataset_mgr <-  dataset_mgr
          }
        } else {
          private$..dataset_mgr <-  DataSetMgr$new()
        }

        private$..dataset_mgr$set(...)

        if(!is.null(file_mgr)) {
          if(inherits(file_mgr, "FileMgr")) {
            file_mgr$dataset_mgr$set(...)
            private$..file_mgr <- file_mgr

          }
        } else {
          private$..file_mgr <- FileMgr$new(dataset_mgr = private$..dataset_mgr)

        }


        #layout_mgr_pvt <- Layout_Mgr$new()
      },

      #' @description
      #' Calculate weighted population percentages for specific variables.
      #' @param ... Variables to pass down to the core percentage processing wrapper.
      #' @param include_n (logical) If `TRUE`, appends structural sample sizes (N)
      #' to the table. Default is `TRUE`.
      #' @return A data frame containing weighted percentages and statistical distributions.
      percents = function(..., include_n = TRUE) {

        wt_var <- self$dataset_mgr$get(weight)
        percents(df = self$prepped_data, inc_n = include_n, ...)

      },

      #' @description
      #' Find column/variable names matching a text or regex pattern.
      #' @param pttrn (character) Regex string match filter. Defaults to `""`.
      #' @return A character vector of matching column names.
      col_names = function(pttrn = "") {

        self$col_info() %>%
          filter(grepl(pttrn, variable)) %>%
          pull(variable)



      },

      #' @description
      #' Extract detailed structural dictionary metadata and labels embedded within columns.
      #' @param section (character) Filter for section names. Defaults to regex `".*"`.
      #' @param label (character) Filter for attribute label descriptors.
      #' Defaults to regex `".*"`.
      #' @return A data frame containing variable definitions, section tags,
      #' and descriptive labels.
      col_info = function(label = ".*", col_name = ".*", section = ".*") {

        df <- self$prepped_data

        ncols <- ncol(df)

        df_atts <- purrr::map(1:ncols, \(icol) {

          df0 <-
            tryCatch(expr = {
              attrs <- attributes(df[[icol]])

              if("variable" %in% names(attrs)) {

                variable      <- attrs[["variable"]] %||% NA
                section_type  <- attrs[["section_type"]] %||% NA
                section_num   <- attrs[["section_num"]] %||% NA %>% as.character()
                section_index <- attrs[["section_index"]] %||% NA %>% as.character()
                section_name  <- attrs[["section_name"]] %||% NA
                label         <- attrs[["label"]] %||% NA

                data.frame(variable, section_type,
                           section_num, section_index,
                           section_name, label)
              } else {

                data.frame()
              }

            }, error = function(e) {
              #browser()
            }
            )

          df0
        }) %>%
          bind_rows()

        df_atts %>%
          filter(grepl(section, section_name, ignore.case = T)) %>%
          filter(grepl(.env$label, label, ignore.case = T)) %>%
          filter(grepl(.env$col_name, variable, ignore.case = T))

      },

      #' @description
      #' Export the active internal data frame out to a standard tabular CSV text file.
      #' @param filename (character) Complete string path location where the CSV file
      #' should be written.
      to_csv = function(filename) {

        if(missing(filename)) {
          stop("You must specify a filename")
          return()
        }
        df <- self$data()
        df %>% write.csv(filename)

      },

      #' @description
      #' Profile the local workspace file system for existing RDS cache files.
      #' @return A tracked summary data frame sorted chronologically by year, geography,
      #' and code layout variants.
      my_local_data = function() {

        p <- private

        params <- p$..dataset_mgr$as.list()

        dir <- p$..file_mgr$apply("data_folder")

        df <- list.files(dir, pattern = "^.._20[0-9]{2}.rds$", recursive = TRUE) %>%
          grep("^[0-9]{4}/local/", ., value = TRUE) %>%
          grep("../(sas|ascii)",., value = TRUE) %>%
          as.data.frame() %>%
          rename(filename = 1) %>%
          mutate(base = basename(filename)) %>%
          mutate(geog = gsub("(..)_([0-9]{4}).*", "\\1", base))%>%
          mutate(year = gsub("(..)_([0-9]{4}).*", "\\2", base))%>%
          mutate(source = gsub(".*(ascii|sas).*", "\\1", filename)) %>%
          relocate(filename, .after = last_col())

        df_ascii <- df %>% filter(source == "ascii")
        df_sas <- df %>% filter(source == "sas")

        df_sas_only <- df_sas %>% anti_join(df_ascii %>% select(year), by = join_by(year))
        df_ascii_only <- df_ascii %>% anti_join(df_sas %>% select(year), by = join_by(year))

        df_ascii %>%
          bind_rows(df_sas_only) %>%
          arrange(year)
      },



      #' @description
      #' Scan local data structures to locate public state data frames that have been cataloged.
      #' @param year (integer) Optional explicit year restriction filter.
      #' @return A structural dataframe highlighting years, geographies, and storage types.
      my_public_geogs = function(year = NULL) {

        p <- private

        params <- p$..dataset_mgr$as.list()

        dir <- p$..file_mgr$apply("data_folder")

        df <-  list.files(dir, recursive = TRUE) %>%
          grep("^[0-9]{4}/public/states", ., value = TRUE) %>%
          grep(".*states/../(sas|ascii)",., value = TRUE) %>%
          as.data.frame() %>%
          rename(filename = 1) %>%
          mutate(test = gsub("/public/states", "", filename)) %>%
          mutate(year = gsub("([0-9]{4}).*", "\\1", test)) %>%
          mutate(geog = gsub("([0-9]{4})/(.*?)/.*", "\\2", test)) %>%
          mutate(source = gsub("([0-9]{4})/(.*?)/(.*?)/.*", "\\3", test)) %>%
          mutate(file = gsub("([0-9]{4})/(.*?)/(.*?)/(.*)", "\\4", test)) %>%
          mutate(version = gsub(".*[0-9]{4}(.*)[.]rds", "\\1", file)) %>%
          mutate(version = as.integer(gsub("_V", "", version))) %>%
          select(-filename, -test) %>%
          relocate(version, .before = file)

        if(!is.null(year)) {
          df <- df %>% filter(year == .env$year)
        }

        df
      },

      #' @description
      #' Read the structured BRFSS dataset from disk with custom attributes appended.
      #' @return An object of class `brfss_data` and `data.frame` complete with configuration tracking keys.
      data = function() {

        fname <- private$..data_path(rw = 'r')

        if(is.null(fname))  return(NULL)

        df_brfss<- readRDS(fname)

        params <- private$..dataset_mgr$patterns

        structure(df_brfss,
                  class = c("brfss_data", "data.frame"),
                  year = params["YEAR"] %>% as.integer() %>% unname(),
                  geog = params["GEOG"] %>% unname(),
                  source = params["SRC"] %>% unname(),
                  extent = params["EXT"] %>% unname(),
                  version = params["VERS"] %>% unname()
        )
      },

      #' @description
      #' Combine multiple years of data for a single column returning a reweighted data frame based on
      #'  original weights and the weight of each year
      #'  (# of completes for that year/total # of completes for all years())
      #' @return An object of class `brfss_data` and `data.frame` complete with configuration tracking
      #' keys like FINAL_WT and _STSTR
      combine_years = function(years, col, subvars, wide = FALSE) {

        yr0 <- head(years, 1)
        yr1 <- tail(years, 1)

        if(yr0 == yr1) {

          stop("This function is for multiple year calculations. Only 1 year was provided")
          return(NULL)

        }


        dsm <- private$..dataset_mgr

        year_save <- dsm$get(year)

        # dm <- DataMgr$new(dataset_mgr = dsm)
        #
        final_atts <- list()

        df_data <- purrr::map(years, \(year) {

          dsm$set(year = year)

          col <- self$col_names(col)
          cols <- c(col, subvars)

          df <- self$prepped_data %>%
            mutate(year = year) %>%
            select(year, all_of(cols), `_LLCPWT`, `_STSTR`)

          purrr::walk(cols, \(.col) {

            nms <- names(attributes(df[[.col]]))

            rm_attrs <- setdiff(nms, c("class", "levels"))

            if(year == yr1 && col == .col) {

              final_atts <<- attributes(df[[.col]])[c("label", "variable", "population")]
            }

            purrr::walk(rm_attrs, \(attr) attr(df[[.col]], attr) <<- NULL)
          })

          attributes(df$`_LLCPWT`) <- NULL
          attributes(df$`_STSTR`) <- NULL

          df


        }) %>%
          bind_rows()%>%
          filter(!is.na(.data[[col]]))

        df_comb <- df_data %>% private$..reweight(year, trim = !wide)
        #
        # df_tots <- df_comb %>% count(year) %>% mutate(pct = n / sum(n))
        #
        # df_comb <- df_comb %>%
        #   left_join(df_tots, by = join_by(year)) %>%
        #   mutate(FINAL_WT = `_LLCPWT` * pct)
        #
        purrr::iwalk(final_atts, \(att, nm) { attr(df_comb, nm) <<- att})

        dsm$set(year = year_save)

        df_comb

      },

      #' @description
      #' Combine multiple versions of data for a single column returning a reweighted data frame based on
      #'  original weights and the weight of each version
      #'  (# of completes for that year/total # of completes for all years())
      #' @return An object of class `brfss_data` and `data.frame` complete with configuration tracking
      #' keys like FINAL_WT and _STSTR
      #'
      combine_versions = function(col, subvars, wide = FALSE) {


        dsm <- private$..dataset_mgr


        # dm <- DataMgr$new(dataset_mgr = dsm)
        #
        final_atts <- list()

        df_data <- purrr::map(0:4, \(version) {

          dsm$set(version = version)

          df <- self$prepped_data

          if(!is.null(df)) {

            df <- df %>%
              mutate(version = version) %>%
              select(version, all_of(cols), `_LLCPWT`, `_STSTR`)

            purrr::walk(cols, \(.col) {

              nms <- names(attributes(df[[.col]]))

              rm_attrs <- setdiff(nms, c("class", "levels"))

              if(col == .col) {

                final_atts <<- attributes(df[[.col]])[c("label", "variable", "population")]
              }

              purrr::walk(rm_attrs, \(attr) attr(df[[.col]], attr) <<- NULL)
            })

            attributes(df$`_LLCPWT`) <- NULL
            attributes(df$`_STSTR`) <- NULL
          }
          df


        }) %>%
          bind_rows()%>%
          filter(!is.na(.data[[col]]))

        df_comb <- df_data %>% private$..reweight(version, trim = !wide)
        #
        # df_tots <- df_comb %>% count(year) %>% mutate(pct = n / sum(n))
        #
        # df_comb <- df_comb %>%
        #   left_join(df_tots, by = join_by(year)) %>%
        #   mutate(FINAL_WT = `_LLCPWT` * pct)
        #
        purrr::iwalk(final_atts, \(att, nm) { attr(df_comb, nm) <<- att})

        dsm$set(version = 0)

        df_comb

      },


      #' @description
      #' Pass updates directly down to the internal dataset configurations framework.
      #' @param ... Named attribute-value pairs to set.
      set = function(...) {

        private$..dataset_mgr$set(...)
      },

      #' @description
      #' Retrieve parameters directly out of the internal dataset configuration framework.
      #' @param ... Named variable values to request.
      get = function(...) {

        private$..dataset_mgr$get(...)
      }


    ),

    active = list(

      #' @field modules Read-only list containing the current year's BRFSS module matrix definitions.
      modules = function(value) {

        if(!missing(value)) {
          stop("this property is read-only")
          return(NULL)
        }

        params <- private$..dataset_mgr$patterns

        file <- private$..file_mgr$apply("brfss_modules_path")

        readRDS(file)

      },

      #' @field has_data Read-only indicator checking if an annual dataset matches the path profile.
      has_data = function(value) {

        if(!missing(value)) {
          stop("this property is read-only")
          return(NULL)
        }

        p <- private

        file <- p$..file_mgr$apply("annual_data_path")

        #message(paste0("looking for data in ", file))
        file.exists(file)

      },

      #' @field has_raw_data Read-only indicator checking for raw localized dataset files on disk.
      has_raw_data = function(value) {

        if(!missing(value)) {
          stop("this property is read-only")
          return(NULL)
        }

        p <- private

        file <- p$..file_mgr$apply("annual_data_path")

        file.exists(file)

      },

      #' @field prepped_data Read-only binding that executes data cleaning routines and transforms inputs into factors.
      #'
      prepped_data = function(value) {

        if(!missing(value)) {
          stop("this property is read-only")
          return(NULL)
        }

        file <-file_mgr$apply("prepped_path")

        readRDS(file =  file)


      },

      # This is the old prepped on request ... now prepped is pre-prepped and saved under the data folder
      # prepped_data = function(value) {
      #
      #   if(!missing(value)) {
      #     stop("this property is read-only")
      #     return(NULL)
      #   }
      #
      #   params <-  private$..dataset_mgr$as.list()
      #
      #   func_any <- paste0("prepped_", params$year)
      #   func_geog <- paste0("prepped_", params$year, "_", params$geog)
      #
      #   df <- self$data()
      #
      #
      #   if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>%
      #     which()
      #
      #   if(is.null(df) || length(f) == 0) {
      #
      #     source <- private$..dataset_mgr$get(source)
      #
      #     try_source <- ifelse (source == "ascii", "sas", "ascii") %>% unname()
      #
      #     private$..dataset_mgr$set(source = try_source)
      #
      #     df <- self$data()
      #
      #     private$..dataset_mgr$set(source = source)
      #   }
      #
      #   if(is.null(df))  return(NULL)
      #
      #   attribs <-  attributes(df)
      #   attribs <-  attribs[!names(attribs) %in% c("row.names", "names",
      #                                              "class")]
      #   if(func_any %in% ls(.GlobalEnv)) {
      #     df <- do.call(func_any, args = list(df))
      #   }
      #
      #   if(func_geog %in% ls(.GlobalEnv)) {
      #
      #     df <- do.call(func_geog,args = list(df))
      #   }
      #
      #   do.call(structure,
      #           args = c(list(df,
      #                         class = c("brfss_prepped", "data.frame")), attribs))
      # },

      #' @field data_file Read-only name of data file
      #'
      data_file = function(value) {

        if(!missing(value)) {
          stop("This property is read-only")
          return(NULL)
        }

        return(private$..data_path())

      },

      #' @field params Read-only snapshot list of parameters tracked inside the inner configuration state.
      params = function(value) {

        if(!missing(value)) {
          stop("this property is read-only")
          return(NULL)
        }

        private$..dataset_mgr$as.list()

      },

      #' @field year Accessor and mutator string key referencing the target study cycle year.
      year = function(value) {

        if(missing(value)) {
          return(self$dataset_mgr$get(year))
        } else {
          if(is.numeric(value)) {
            self$dataset_mgr$set(year = value)
          }
        }
      },

      #' @field dataset_mgr Active instance of the dataset parameter configuration manager.
      dataset_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$..dataset_mgr <-  value
            private$..file_mgr <- FileMgr$new(dataset_mgr = value)
          }
        }
        private$..dataset_mgr

      },


      #' @field file_mgr Active instance of the file path conversion manager.
      file_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "FileMgr")) {
            private$..file_mgr <- FileMgr$new(dataset_mgr = value)
          }
        }
        private$..file_mgr

      }


    )
  )



#' Public Data Manager Class
#'
#' @description
#' Extended manager designed for processing public-release BRFSS components, handling state subsets,
#' and isolating state-level sample sizes.
#'
#' @seealso Inherits directly from \code{\code{\link{DataMgr}}}.
#' @export
PublicDataMgr <-
  R6::R6Class(
    classname = "PublicDataMgr",
    inherit = DataMgr,

    private = list(
    ),

    public = list(

      initialize = function(...) {

        super$initialize(extent = "public", ...)

      },

      #' @description
      #' Retrieve public survey tables, handling automatic falling back filtering
      #' based on state FIPS boundaries.
      #' @param any (logical) If `TRUE`, falls back to checking state-wide scopes if localized
      #'  file definitions are missing. Default is `TRUE`.
      #' @return A data frame containing public survey entries.
      data = function(any = TRUE) {

        df <- super$data()

        if(is.null(df) && any) {

          geog_flag <- super$dataset_mgr$get(geog_flag)

          if(geog_flag == "on") {

            geog <- super$dataset_mgr$get(geog)
            gm <- GeogMgr$new(geog = geog)
            fips <- gm$geog %>% pull(fips)
            super$dataset_mgr$set(geog_flag = "off")
            df <- super$data() %>% filter(`_STATE` == fips)
            super$dataset_mgr$set(geog_flag = "on")

          }
        }

        df
      },

      #' @description
      #' Extract, isolate, and save a specific geographical slice out to a dedicated
      #' localized file.
      #' @param geog (character) Geographic identifier (e.g., FIPS, abbreviation, or name string).
      save = function(geog = NULL) {

        if(!is.null(geog)) {

          dataset_mgr <- super$dataset_mgr
          file_mgr <- FileMgr$new(dataset_mgr = dataset_mgr)

          geog_flag <- dataset_mgr$get(geog_flag)

          dataset_mgr$set(geog_flag = "off")
          gm <- GeogMgr$new(geog = geog)
          fips <- gm$geog %>% pull(fips)

          if(!is.null(fips)) {
            dataset_mgr$set(geog_flag = "off")
            df <- super$data() %>% filter(`_STATE` == fips)

            dataset_mgr$set(geog = geog)

            file <- file_mgr$apply("annual_data_path")
            df %>% saveRDS(file)
            dataset_mgr$set(geog_flag = geog_flag)
          }

        }
      }

    ),

    active = list(

      #' @field geog Active binding to get or alter the regional targeting configuration parameter.
      geog = function(value) {

        if(missing(value)) return(super$dataset_mgr$get(geog))

        super$dataset_mgr$set(geog = value)
      }
    )
  )

#' Local Data Manager Class
#'
#' @description
#' Extended manager subclass specialized in handling local or regional-level subsets of BRFSS data.
#' Automatically configures and targets data structures with an extent set to "local".
#'
#' @seealso Inherits directly from \code{\link{DataMgr}}.
#' @export
LocalDataMgr <-
  R6::R6Class(
    classname = "LocalDataMgr",
    inherit = DataMgr,

    private = list(
    ),

    public = list(

      initialize = function(...) {

        super$initialize(extent = "local", ...)

      }
    ),

    active = list(

    )
  )


