
#' Data_Mgr R6 Class
#'
#' @export
DataMgr <-
  R6::R6Class(
    classname = "DataMgr",

    private = list(
      layout_mgr_pvt = NULL,
      dataset_mgr_pvt = NULL,
      file_mgr_pvt = NULL,

      data_path = function(rw = c("r","w")) {

        p <- private

        read <- rw == 'r'
        write <- rw == 'w'

        params <- p$dataset_mgr_pvt$patterns

        fldr <- p$file_mgr_pvt$apply("brfss_annual_data_folder")

        if(!dir.exists(fldr) && write) {
          dir.create(fldr, recursive = TRUE)
        }

        file <- p$file_mgr_pvt$apply("brfss_annual_data_file")

        path <- paste0(fldr,file)

        if(read && !file.exists(path)) path <- NULL

        path

      }

    ),

    public = list(

      initialize = function(dataset_mgr = NULL, file_mgr = NULL) {

        if(!is.null(dataset_mgr)) {
          if(inherits(dataset_mgr, "DataSetMgr")) {
            private$dataset_mgr_pvt <-  dataset_mgr
          }
        } else {
          private$dataset_mgr_pvt <-  DataSetMgr$new()
        }

        if(!is.null(file_mgr)) {
          if(inherits(file_mgr, "BRFSS_FileMgr")) {
            private$file_mgr_pvt <- BRFSS_FileMgr$new(private$dataset_mgr_pvt)
          }
        } else {
          private$file_mgr_pvt <- BRFSS_FileMgr$new(private$dataset_mgr_pvt)

        }
        #layout_mgr_pvt <- Layout_Mgr$new()
      },

      percents = function(..., include_n = TRUE) {

        wt_var <- self$dataset_mgr$get(weight)
        brfss::percents(df = self$prepped_data, inc_n = include_n, ...)

      },

      col_names = function(pttrn = "") {

        self$col_info() %>%
          filter(grepl(pttrn, variable)) %>%
          pull(variable)



      },

      col_info = function(section = ".*", label = ".*") {

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
              browser()
            }
            )

          df0
        }) %>%
          bind_rows()

        df_atts %>%
          filter(grepl(section, section_name)) %>%
          filter(grepl(.env$label, label))

      },

      my_local_data = function() {

        p <- private

        params <- p$dataset_mgr_pvt$as.list()

        dir <- p$file_mgr_pvt$apply("data_folder")

        df <- list.files(dir, pattern = "^.._20[0-9]{2}.rds$", recursive = TRUE) %>%
          grep("^[0-9]{4}/local/", ., value = TRUE) %>%
          grep("../(sas|ascii)",., value = TRUE) %>%
          as.data.frame() %>%
          rename(filename = 1) %>%
          mutate(base = basename(filename)) %>%
          mutate(geog = gsub("(..)_([0-9]{4}).*", "\\1", base))%>%
          mutate(year = gsub("(..)_([0-9]{4}).*", "\\2", base))%>%
          mutate(source = gsub(".*(ascii|sas).*", "\\1", filename))

        df_ascii <- df %>% filter(source == "ascii")
        df_sas <- df %>% filter(source == "sas")

        df_sas_only <- df_sas %>% anti_join(df_ascii %>% select(year), by = join_by(year))
        df_ascii_only <- df_ascii %>% anti_join(df_sas %>% select(year), by = join_by(year))

        df_ascii %>%
          bind_rows(df_sas_only) %>%
          arrange(year)
      },

      my_public_geogs = function(year = NULL) {

        p <- private

        params <- p$dataset_mgr_pvt$as.list()

        dir <- p$file_mgr_pvt$apply("data_folder")

        df <- list.files(dir, recursive = TRUE) %>%
          grep("^[0-9]{4}/public/states", ., value = TRUE) %>%
          grep(".*states/../(sas|ascii)",., value = TRUE) %>%
          gsub("/.._[0-9]{4}.*", "", .)%>%
          gsub("/public/states", "", .) %>%
          as.data.frame() %>%
          rename(dir = 1) %>%
          mutate(year = gsub("([0-9]{4}).*", "\\1", dir))%>%
          mutate(geog = gsub("([0-9]{4})/(..)/(.*)", "\\2", dir))%>%
          mutate(source = gsub("([0-9]{4})/(..)/(.*)", "\\3", dir)) %>%
          select(-dir)

        if(!is.null(year)) {
          df <- df %>% filter(year == .env$year)
        }

        df
      }

    ),

    active = list(

      modules = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        params <- private$dataset_mgr_pvt$patterns

        file <- private$file_mgr_pvt$apply("brfss_modules_path")

        readRDS(file)

      },

      has_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        !is.null(self$prepped_data)


      },

      prepped_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        params <-  private$dataset_mgr_pvt$as.list()

        func_any <- paste0("prepped_", params$year)
        func_geog <- paste0("prepped_", params$year, "_", params$geog)

        df <- self$data

        if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>%
          which()

        if(is.null(df) || length(f) == 0) {

          source <- private$dataset_mgr_pvt$get(source)

          try_source <- ifelse (source == "ascii", "sas", "ascii") %>% unname()

          private$dataset_mgr_pvt$set(source = try_source)

          df <- self$data

          private$dataset_mgr_pvt$set(source = source)
        }

        if(is.null(df))  return(NULL)

        attribs <-  attributes(df)
        attribs <-  attribs[!names(attribs) %in% c("row.names", "names",
                                                   "class")]

        if(func_any %in% ls(.GlobalEnv)) {
          df <- do.call(func_any, args = list(df))
        }

        if(func_geog %in% ls(.GlobalEnv)) {

          df <- do.call(func_geog,args = list(df))
        }

        do.call(structure,
                args = c(list(df,
                              class = c("brfss_prepped", "data.frame")), attribs))
      },

      params = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        private$dataset_mgr_pvt$as.list()

      },

      year = function(value) {

        if(missing(value)) {
          return(self$dataset_mgr$get(year))
        } else {
          if(is.numeric(value)) {
            self$dataset_mgr$set(year = value)
          }
        }
      },


      dataset_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$dataset_mgr_pvt <-  value
            private$file_mgr_pvt <- BRFSS_FileMgr$new(value)
          }
        }
        private$dataset_mgr_pvt

      },

      data = function() {

        params <- private$dataset_mgr_pvt$as.list()

        fname <- private$data_path(rw = 'r')

        if(is.null(fname))  return(NULL)

        df_brfss<- readRDS(fname)

        params <- private$dataset_mgr_pvt$patterns

        structure(df_brfss,
                  class = c("brfss_data", "data.frame"),
                  year = params["YEAR"] %>% as.integer() %>% unname(),
                  geog = params["GEOG"] %>% unname(),
                  source = params["SRC"] %>% unname(),
                  extent = params["EXT"] %>% unname(),
                  version = params["VERS"] %>% unname()
        )
      }


    )
  )

