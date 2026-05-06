

Download_Mgr <- R6::R6Class(
  classname = "Download_Mgr",

  private = list(
    dataset_mgr_pvt = NULL,
    file_mgr = NULL,
    verbose_pvt = FALSE

  ),

  public = list(

    initialize = function(dataset_mgr = NULL) {

      private$dataset_mgr_pvt <- dataset_mgr
      private$file_mgr <- FileMgr$new(dataset_mgr = dataset_mgr)
    },

    dl_metadata = function(fldrout = NULL) {

      p <- private

      if(p$verbose_pvt) cat(" ... downloading ... metadata ... ")
      download_metadata(file_mgr = p$file_mgr, folderout = fldrout)

          },

    dl_codebook = function(fldrout = NULL) {

      if(private$verbose_pvt) cat(" ... downloading ... codebook ... \n")

      cb_mgr <- CodebookMgr$new(dataset_mgr = private$dataset_mgr_pvt)

      cb_mgr$download_codebook()
    },

    dl_data = function(fldrout = NULL) {

      p <- private

      params <- p$dataset_mgr_pvt$as.vector()
      source <- params["source"]
      extent <- params["extent"]

      if(extent != "public") {

        stop("extent must be 'public'. local data are not downloaded from the CDC BRFSS website.")

      }

      if(p$verbose_pvt) cat(" ... downloading ...", source, "data ... ")


      if(source == "ascii"){
        download_ascii_data(file_mgr = p$file_mgr)
      } else if(source == "sas") {
        download_sas_data(file_mgr = p$file_mgr)
      }

      return(invisible(NULL))

    }


  ),

  active = list(

    dataset_mgr = function(value) {

      if(!missing(value)) {

        if(inherits(value, "DataSetMgr")) private$dataset_mgr_pvt <- value

      } else {

        return(private$dataset_mgr_pvt)

      }

    },

    verbose = function(value) {

      if(missing(value)) {
        return(private$verbose_pvt)
      } else {
        if(!class(value) == "logical") {
          warning("This property requires a logical value")
          return(invisible())
        }

        private$verbose_pvt <- value

        return(invisible(NULL))
      }


    }

  )
)
