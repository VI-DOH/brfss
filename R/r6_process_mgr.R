
#' Process Manager for BRFSS Data
#'
#' @description
#' A class to manage the processing pipeline for BRFSS datasets, including
#' SAS layouts, ASCII conversion, and attribute assignment.
#'
#' @export
Process_Mgr <- R6::R6Class(
  classname = "Process_Mgr",

  private = list(
    convert_pvt = TRUE,
    attribs_pvt = TRUE,
    saq_pvt = FALSE,
    split_pvt = TRUE,
    responses_pvt = TRUE,
    factorize_pvt = TRUE,
    verbose_pvt = FALSE,

    file_mgr_pvt = NULL,
    dataset_mgr_pvt = NULL,

    brfss_data_path = function(rw = c("r","w")) {

      read <- rw == 'r'
      write <- rw == 'w'

      fldr <- private$file_mgr_pvt$apply("brfss_annual_data_folder")

      if(!dir.exists(fldr) && write) {
        dir.create(fldr, recursive = TRUE)
      }

      file <- private$file_mgr_pvt$apply("brfss_annual_data_file")

      path <- paste0(fldr,file)

      if(read && !file.exists(path)) path <- NULL

      path
    }


  ),

  public = list(

    initialize = function(dataset_mgr = NULL,
                          convert = TRUE,
                          attribs = TRUE,
                          saq = FALSE,
                          split = TRUE,
                          responses = TRUE,
                          factorize = TRUE,
                          verbose = FALSE) {

      private$file_mgr_pvt <- brfss::FileMgr$new(dataset_mgr)
      private$dataset_mgr_pvt <- private$file_mgr_pvt$dataset_mgr$clone()
      private$file_mgr_pvt$dataset_mgr <- private$dataset_mgr_pvt


      private$dataset_mgr_pvt$set_legacy()

      convert_pvt <- convert
      attribs_pvt <- attribs
      saq_pvt <- saq
      split_pvt <- split
      responses_pvt <- responses
      factorize_pvt <- factorize
      verbose_pvt <- verbose
    },

    process_sas_layout = function() {

      brfss::save_sas_layout()

    },

    process_codebook = function(){

      brfss::process_codebook()
    },

    process_ascii = function(verbose = FALSE) {

      self$convert_ascii(verbose = verbose)

      brfss::add_column_attributes(verbose = verbose)
      brfss::factorize(verbose = verbose)

    },

    add_column_attributes = function() {

      brfss::add_column_attributes(verbose = private$verbose_pvt)

    },

    convert_ascii = function(main = TRUE, versions = TRUE, layout = NULL) {

      ds_mgr <- private$dataset_mgr_pvt
      file_mgr <- private$file_mgr_pvt
      verbose <- private$verbose_pvt

      if(df_mgr$get(source) == "sas") {
        df_mgr$set(source = "ascii")
        message("Setting dataset source to asciii")
      }

      if(is.null(layout)) {

        lo_mgr <- Layout_Mgr$new()
        layout <- lo_mgr$get_codebook_layout()

      }

      if(is.null(layout)) {
        return (NULL)
      }

      if(main) version <- 0 else {
        if(versions) version = 1 else return()
      }
      ds_mgr$set(version = version)

      file_raw <- file_mgr$apply("ascii_filename_raw")
      path_raw <- file_mgr$apply("ascii_path_raw")

      while (file.exists(path_raw)) {

        if(verbose) cat("... reading version [", version, "] : ", path_raw, "\n")



        df <- self$read_ascii(filename = path_raw, layout = layout, verbose = verbose)

        path <- file_mgr$apply("brfss_annual_data_path")
        #    path_unf <-  file_mgr$apply("brfss_annual_data_unfactored_path")


        if(!dir.exists(dirname(path))) dir.create(dirname(path))


        if(verbose) cat("... writing ", path,"\n")


        saveRDS(df, file = path)
        #    saveRDS(df, file = path_unf)

        version <- version + 1
        ds_mgr$set(version = version)

        path_raw <- file_mgr$apply("ascii_path_raw")

      }

      ds_mgr$set(version = 0)

    },

    read_ascii = function(filename = path_raw, layout = layout, verbose = verbose) {


      df_cb <- layout %>%
        filter(!grepl("DUMMY", col_name))

      col_names  <-  df_cb %>% pull(col_name)

      cols <- readr::fwf_positions(
        start = df_cb %>% pull(col_start),
        end   = df_cb %>% pull(col_end),
        col_names = col_names
      )

      col_types <- df_cb %>% pull(var_type) %>% tolower() %>% substring(1,1)

      df <- readr::read_fwf(file = filename, col_positions = cols,
                            readr::cols(.default = "c") ) %>%
        readr::type_convert()

      df
    },

    read_xpt = function(version = 0, verbose = F) {

      ds_mgr <- private$dataset_mgr_pvt
      file_mgr <- private$file_mgr_pvt
      verbose <- private$verbose_pvt

      extent <- ds_mgr %>% get("extent")
      source <- ds_mgr %>% get("source")

      if(source != "sas") {
        df_mgr$set(source = "sas")
        source <- "sas"
        message("Setting dataset source to sas")
      }

      ds_mgr$set(version = version)

      if(extent == "public")  ds_mgr$set(GFLAG = "off")

      ########################################################################%%%%%%%%%
      ##
      ##    If file and folder names not supplied, create them from the file patterns

      ##
      ##    get sasout location
      ##
      sasout_file <- file_mgr$apply("sas_sasout_path")

      ##
      ##    get xpt raw data location
      ##

      xpt_file <- file_mgr$apply("xpt_path")


      ##
      ##  get save file (.rdata) location
      ##

      save_file<- file_mgr$apply("brfss_annual_data_path")

      ##
      ##    read the xpt files
      ##

      if(version>0) {
        if(verbose) cat("Trying version ",version,"\n")
      } else {
        if(verbose) cat("Trying main file \n")
      }

      if(file.exists(xpt_file)) {
        if(verbose) cat("Reading ",xpt_file,"\n")
        df_xpt<- haven::read_xpt(xpt_file)

        # st <- fips::state_fips(geog)
        # df_xpt <- df_xpt %>% filter(`_STATE` == st)

        cat("Getting sasout\n")


        #df_xpt <- df_xpt %>% add_col_attributes()

        if(!dir.exists(dirname(save_file))) dir.create(dirname(save_file),recursive = T)

        saveRDS(df_xpt,file = save_file)

        return(TRUE)
      } else {
        if(verbose) cat(xpt_file," doesn't exist\n")
        return(FALSE)
      }
    },

    convert_sas = function(verbose = FALSE) {

      self$read_xpt(version = 0)

      cont <- TRUE

      ivers <- 1
      if(verbose) cat(" ... trying versions\n ")
      while (cont) {

        if(verbose) cat("Converting version=", ivers, "\n")

        cont <- self$read_xpt(version=ivers, verbose=TRUE)


        ivers <- ivers + 1
      }

      private$dataset_mgr$set(version = 0)

      brfss::add_column_attributes(verbose = verbose)
      brfss::factorize(verbose = verbose)

    },

    calc_stats = function() {

      if(responses) {
        brfss:save_response_stats()
        brfss:save_module_stats()
      }
    },

    split_geogs = function(geogs = NULL,  main=TRUE, versions=TRUE, factorize = TRUE, verbose=TRUE) {

      brfss::split_geogs(geogs = geogs, main = main, versions = versions,
                         factorize = factorize, verbose = verbose)
    },

    make_factors = function(main=TRUE, versions=TRUE, verbose=FALSE) {

      brfss::factorize(main = main, versions = versions, verbose = verbose)
      # make sure some version will be split

    }

  ),

  active = list(

    convert = function(value) {

      if(missing(value)) return(private$convert_pvt)


    },

    attribs = function(value) {

      if(missing(value)) return(private$attribs_pvt)


    },

    saq = function(value) {

      if(missing(value)) return(private$saq_pvt)


    },

    split = function(value) {

      if(missing(value)) return(private$split_pvt)


    },

    responses = function(value) {

      if(missing(value)) return(private$responses_pvt)


    },

    factorize = function(value) {

      if(missing(value)) return(private$factorize_pvt)


    },

    verbose = function(value) {

      if(missing(value)) return(private$verbose_pvt)


    },

    dataset_mgr = function(value) {

      if(missing(value)) return(private$dataset_mgr_pvt)

      if(!inherits(value, "DataSetMgr")) {
        message("value must be a DataSetMgr object")
        return(NULL)
      }

      file_mgr$dataset_mgr <- value
      private$dataset_mgr_pvt <- file_mgr$dataset_mgr
    }

  )
)
