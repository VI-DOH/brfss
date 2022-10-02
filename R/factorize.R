
#' Factorize BRFSS Data by Geography
#'
#' Add factors to appropriate columns in the geographies of interest.
#'
#' @param main logical - process main XPT file
#' @param versions logical - process all versioned XPT/ASC files
#' @param verbose logical - provide details during processing
#' @param progress function - to show progress
#' @export
#' @examples
#'
#'\dontrun{
#' factorize(main=TRUE, versions=TRUE,verbose=TRUE)
#'}
#'
#'
factorize<-function(main=TRUE, versions=TRUE, verbose=TRUE, progress = NULL) {

  # make sure some version will be split

  if(!(main || versions)) return(NULL)
  ver<-integer(0)
  if(main) ver<-0

  vermax <- highest_version()
  if(vermax == 0) versions = FALSE

  if(versions) ver<-c(ver,1:vermax)

  df_geogs <- get_geogs_all()

  # factorize each version

  sapply(ver,function(version) {

    brfss.param(version = version)
    params <- my.brfss.patterns()

    show_progress(progress,
                  message = paste0("Factorizing ... trying version [", version, "]"))


    ext <- brfss.param(extent)
    brfss.param(extent = "local")

    if(brfss.param(geog) == '*') {
      geogs<-unique(df_brfss$`_STATE`)

    } else {

      geogs <- c(brfss.param(geog),brfss.param(geogs_other))
      if(is.character(geogs)) {
        geogs<-sapply(geogs,function(state) {
          df_geogs[df_geogs$Abbrev==state,"Id"]
        })

        geogs <- unlist(unname(geogs))
      }
    }

    geog_save <- brfss.param(geog)

    mapply(function(id,nm) {
      if(id%in%geogs) {

        brfss.param(geog = nm)
        params <- my.brfss.patterns()

        # get data for the state of interest and make sure there is data

        # make sure, even if temporarily, extent param is set to local
        #     to make sure we are saving the data under the geog folder
        fname <- brfss_data_path( rw = 'w')
        df_state <- data.frame()

        tryCatch(expr = {df_state <- readRDS(file = fname)},
            error =  function(e) e)

        if(nrow(df_state)>0) {
          if(verbose) cat("Factorizing ",nm,"V",version,"\n")

          show_progress(progress,
                        message = paste0("Factorizing ... ", nm, "V", version))

          ##   for now, have to save and (re-)attach the attributes for the columns

            df_state <- df_state %>% make_factors()
          }

          # make sure, even if temporarily, extent param is set to local
          #     to make sure we are saving the data under the geog folder


          fname <- brfss_data_path( rw = 'w')

          if(verbose) cat("Going to save :", fname, "\n")

          show_progress(progress,
                        message = paste0("Factorizing ... saving ", fname))

          saveRDS(df_state,file = fname)

        }
    },df_geogs$Id,df_geogs$Abbrev)

    brfss.param(geog = geog_save)
    brfss.param(extent = ext)


  })


  invisible()
}
