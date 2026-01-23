
#' Data_Mgr R6 Class
#'
#' @export
BRFSS_DataMgr <-
  R6::R6Class(
    classname = "BRFSS_DataMgr",

    private = list(
      params_mgr_pvt = NULL,
      patterns_mgr_pvt = NULL,

      data_path = function(rw = c("r","w")) {

        p <- private

        read <- rw == 'r'
        write <- rw == 'w'

        params <- p$params_mgr_pvt$patterns

        fldr <- p$patterns_mgr_pvt$apply("brfss_annual_data_folder")

        if(!dir.exists(fldr) && write) {
          dir.create(fldr, recursive = TRUE)
        }

        file <- p$patterns_mgr_pvt$apply("brfss_annual_data_file")

        path <- paste0(fldr,file)

        if(read && !file.exists(path)) path <- NULL

        path

      }

    ),

    public = list(

      initialize = function(params_mgr = NULL) {

        if(!is.null(params_mgr)) {
          if(inherits(params_mgr, "BRFSS_Params")) {
            private$params_mgr_pvt <-  params_mgr
            private$patterns_mgr_pvt <- Pattern_Mgr$new(params_mgr)
          }
        }
      }

    ),

    active = list(

      prepped_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        params <-  private$params_mgr_pvt$as.list()

        func_any <- paste0("prepped_", params$year)
        func_geog <- paste0("prepped_", params$year, "_", params$geog)

        df <- self$data

        if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>% which()

        if(is.null(df) || length(f) == 0) {

          source <- private$params_mgr_pvt$get(source)

          try_source <- ifelse (source == "ascii", "sas", "ascii")

          private$params_mgr_pvt$set(source = try_source)

          df <- self$data

          private$params_mgr_pvt$get(source = source)
        }

        if(is.null(df))  return(NULL)

        attribs <-  attributes(df)
        attribs <-  attribs[!names(attribs) %in% c("row.names", "names", "class")]

        if(func_any %in% ls(.GlobalEnv)) {
          df <- do.call(func_any, args = list(df))
        }

        if(func_geog %in% ls(.GlobalEnv)) {

          df <- do.call(func_geog,args = list(df))
        }

        do.call(structure, c(list(df, class = c("brfss_prepped", "data.frame")), attribs))
      },

      params = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        private$params_mgr_pvt$as.list()

      },

      params_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "BRFSS_Params")) {
            private$params_mgr_pvt <-  value
            private$patterns_mgr_pvt <- Pattern_Mgr$new(value)
          }
        }
        private$params_mgr_pvt

      },

      data = function() {

        params <- private$params_mgr_pvt$as.list()

        fname <- private$data_path(rw = 'r')

        if(is.null(fname))  return(NULL)

        df_brfss<- readRDS(fname)

        params <- private$params_mgr_pvt$patterns

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
