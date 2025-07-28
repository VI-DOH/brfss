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

  if(brfss.param(extent) == 'public' && saq) {
    warning("Data are public, saq = TRUE is incongruous, setting saq to FALSE")
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
#' split_geogs( main=TRUE, versions=TRUE,verbose=TRUE)
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

    df_my_geogs <- data.frame(Id = geogs) %>%
      left_join(df_geogs, by = join_by(Id))

    add_cols<-character(0)

    geog_save <- brfss.param(geog)

    geog_type <- df_brfss %>% pull(`_STATE`) %>% class()

    mapply(function(id, abb, nm) {

      brfss.param(geog = abb)
      params <- my.brfss.patterns()

      # get data for the state of interest and make sure there is data

      if(geog_type == "numeric") {
        df_state <- df_brfss %>% filter(`_STATE` == id)
      }  else {

        df_state <- df_brfss %>% filter(`_STATE` == nm)
      }

      if(nrow(df_state)>0) {
        if(verbose) cat("Saving ",abb,"V",version,"\n")

        show_progress(progress,
                      message = paste0("Splitting ... ", abb, "V", version))

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

        fname <- brfss_data_path( rw = 'w')

        # brfss.param(extent = ext)

        if(verbose) cat("Going to save :", fname, "\n")

        show_progress(progress,
                      message = paste0("Splitting ... saving ", fname))

        saveRDS(df_state,file = fname)

      }
    }, df_my_geogs$Id, df_my_geogs$Abbrev, df_my_geogs$Geog)

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

