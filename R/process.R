require(dplyr)


#' Process BRFSS Annual Data Files
#'
#'
#' Use this function to process a single year of BRFSS data. This function sill download, unzip,
#' and create a data frame with all data from the XPT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param source character - data source ('sas' or 'ascii')
#' @param download - logical - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param convert - logical - read the downloaded files into data_frames and save?
#' Useful (set = FALSE) if you already have the downloaded files processed
#' @param codebook - logical - download and process the annual codebook
#' @param attribs - logical - add attributes to each column describing the column's data
#' @param split - logical - split the processed data file by state/geography
#' @param factorize - logical - convert columns to factors where appropriate
#' @param saq - logical - add state-added questions
#' @param progress - function - progress function to pass progress info to
#' @param ... other params
#'
#' @return invisible()
#' @export
#'

process_year <- function( dl_metadata = FALSE, dl_codebook = FALSE,
                          dl_data = FALSE,
                          layout = TRUE, convert=TRUE, codebook = TRUE, attribs = TRUE,
                          split = TRUE, factorize = TRUE, saq = FALSE,
                          responses = TRUE, verbose=FALSE, progress = NULL, ...) {

  # by definition, state-added- questions are local

  if(brfss.param(extent) == 'national' && saq) {
    warning("Data are national, saq = TRUE is incongruous, setting saq to FALSE")
    saq <- FALSE
  }
  # get the path to travel ... sas or ascii

    if(brfss.param(source) == "sas") {

    sas_process_year(dl_metadata = dl_metadata, dl_codebook = dl_codebook,
                     dl_data = dl_data, layout = layout, convert=convert, saq = saq,
                     codebook = codebook, attribs = attribs,
                     split = split, factorize = factorize,
                     responses = responses, verbose=verbose, progress = progress)

  } else {

    ascii_process_year(dl_metadata = dl_metadata, dl_codebook = dl_codebook,
                       dl_data = dl_data, convert=convert, codebook = codebook,
                       layout = layout,
                       attribs = attribs, split = split, factorize = factorize,
                       saq = saq, responses = responses, verbose=verbose, progress)
  }

}


#' Split BRFSS Data by Geography
#'
#' The main BRFSS data file that is created when the downloaded file from the public CDC website
#' is read has data for all geographies, whether the SAS (XPT) file or the ASCII (ASC) file.
#' This function splits out the geographies of interest.
#'
#' @param year integer - year of interest
#' @param source character - data source ('sas' or 'ascii')
#' @param main logical - process main XPT file
#' @param versions logical - process all versioned XPT/ASC files
#' @param my_geog character - abbreviation for primary state/geography of interest (e. "MT")
#' @param other_geogs character - abbreviations for other states/geographies of interest (e. c("ID","WY"))
#' @param verbose logical - provide details during processing
#'
#' @export
#' @examples
#'
#'\dontrun{
#' split_geogs(year = 2020, source = 'sas', main=TRUE, versions=TRUE, my_geog="MT", other_geogs=NULL,verbose=TRUE)
#'}
#'
#'
split_geogs<-function(main=TRUE, versions=TRUE,
                      factorize = FALSE, verbose=TRUE, progress = NULL) {

  # make sure some version will be split

  if(!(main || versions)) return(NULL)
  ver<-integer(0)
  if(main) ver<-0

  vermax <- highest_version()
  if(vermax == 0) versions = FALSE

  if(versions) ver<-c(ver,1:vermax)

  df_geogs <- get_geogs_all()

  sapply(ver,function(version) {

    browser()

    brfss.param(version = version)
    brfss.param(geog_flag = 'off')

    params <- my.brfss.patterns()

    rdata_file <- apply.pattern("brfss_annual_data_path",params)

    show_progress(progress,
                  message = paste0("Splitting ... trying version [", version, "]"))

    df_brfss <- readRDS(file = rdata_file)

    brfss.param(geog_flag = 'on')

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

    add_cols<-character(0)

    geog_save <- brfss.param(geog)

    mapply(function(id,nm) {

      if(id%in%geogs) {

        brfss.param(geog = nm)
        params <- my.brfss.patterns()

        # get data for the state of interest and make sure there is data

        df_state<-df_brfss[df_brfss$`_STATE`==id,]

        if(nrow(df_state)>0) {
          if(verbose) cat("Saving ",nm,"V",version,"\n")

          show_progress(progress,
                        message = paste0("Splitting ... ", nm, "V", version))

          ##   for now, have to save and (re-)attach the attributes for the columns

          sapply(1:ncol(df_brfss),function(i) {
            attrs<-attributes(df_brfss[[i]])

            if(!is.null(attrs)){
              sapply(1:length(attrs),function(j) {

                attr(df_state[[i]],names(attrs[j]))<<-attr(df_brfss[[i]],names(attrs[j]))
              })
            } else {
              add_cols<<-c(add_cols,colnames(df_brfss)[i])
            }

          })
          ##################################################
          ##################################################
          #
          # moved this to a separate function

          # if(factorize) {
          #
          #   show_progress(progress,
          #                 message = paste0("Splitting ... ", nm, "V",
          #                                  version, " ... adding factors"))
          #
          #   df_state <- df_state %>% make_factors()
          # }
          #


          # make sure, even if temporarily, extent param is set to local
          #     to make sure we are saving the data under the geog folder

          # rethinking the above ... locally, you may want to have a
          #   copy of the public data set ...
          #   right now, saving it would overwrite your local copy

          # ext <- brfss.param(extent)
          #
          # brfss.param(extent = "local")

          fname <- brfss_data_path( rw = 'w')

          # brfss.param(extent = ext)

          if(verbose) cat("Going to save :", fname, "\n")

          show_progress(progress,
                        message = paste0("Splitting ... saving ", fname))

          saveRDS(df_state,file = fname)

        }
      }
    },df_geogs$Id,df_geogs$Abbrev)

    brfss.param(geog = geog_save)

  })


  invisible()
}


#' Save BRFSS Data
#'
#' Save BRFSS data frame to a .RData file. This standardizes the object name and the file name
#'
#' @param df data frame - BRFSS data
#' @param year integer - year of interest
#' @param geog character - 2-char state/territory abbreviation
#' @param version integer - version of interest (default = 0)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_brfss <- function(df_brfss, year= 202, geog = "MT")
#' }
save_brfss <- function(df, year= NULL, geog = NULL, version = 0) {

  year <- get.year(year)
  geog <- get.geog(geog)

  #fname <- brfss_data_path(year = year, geog = geog, version = version, rw = 'w')
  fname <- brfss_data_path( rw = 'w')

  saveRDS(df,file = fname)

}

#########################################################################################
##
##    TODO: means to add state-specific columns and or processing -
##
##        eg. in the VI CTYCODE is the island so mutate the column (or a new column) and
##             apply the factors
##              create your state=specific age categories
##
##########################################################################################

#########################################################################################
##
##    TODO: means to add state-specific layout
##
##########################################################################################

#########################################################################################
##
##    TODO: means to combine versions
##
##########################################################################################

