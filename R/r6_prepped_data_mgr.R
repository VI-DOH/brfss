
library(R6)

PreppedDataMgr <-
  R6::R6Class(
    classname = "PreppedDataMgr",

    private = list(

      data_mgr_pvt = NULL

    ),

    public = list(

      initalize = function(data_mgr) {

        if(missing(data_mgr) || !inherits(data_mgr, "DataMgr")) {

          message("A valid data manager object is required")
          return(NULL)
        }

        private$data_mgr_pvt = data_mgr
      },

      prepped_data = function() {

        brfss.params(...)
        #if(year == 2024) browser()

        params <- as.list(brfss::brfss.params())

        func_any <- paste0("prepped_", params$year)
        func_geog <- paste0("prepped_", params$year, "_", params$geog)

        df <- brfss_data()

        if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>% which()

        if(is.null(df) || length(f) == 0) {

          source <- brfss.param(source)
          try_source <- ifelse (source == "ascii", "sas", "ascii")

          brfss.param(source = try_source)

          df <- brfss_data()

          brfss.param(source = source)
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
      }
    ),

    active = list(


    )




  )



