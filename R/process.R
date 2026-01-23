


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
split_geogs<-function(geogs = NULL, main=TRUE, versions=TRUE, col_atts = TRUE,
                      factorize = TRUE, verbose=TRUE, progress = NULL) {

  # make sure some version will be split

  if(brfss.param(extent) != "public") {

    warning("extent must be 'public' to split geographies")
    return(NULL)
  }

  if(!(main || versions)) return(NULL)

  ver <- 0:highest_version()
  if(!main) ver <- tail(ver,-1)
  if(!versions) ver = head(ver,1)


  df_my_geogs <- geog_info(geogs)

  if(is.null(df_my_geogs) || nrow(df_my_geogs) == 0) return(NULL)

  purrr::walk(ver,function(version) {

    brfss.param(version = version)
    brfss.param(geog_flag = 'off')

    params <- my.brfss.patterns()

    rdata_file <- apply.pattern("brfss_annual_data_path", params)

    if(verbose) cat(paste0("Splitting ... trying version [", version, "]\n"))

    df_brfss <- readRDS(file = rdata_file)

    brfss.param(geog_flag = 'on')

    add_cols<-character(0)

    geog_save <- brfss.param(geog)

    geog_type <- df_brfss %>% pull(`_STATE`) %>% class()

    purrr::pwalk(df_my_geogs, function(Geog, Id, Abbrev) {

      brfss.param(geog = Abbrev)
      params <- my.brfss.patterns()

      df_state <- df_brfss %>% split_geog(geog = Abbrev, col_atts = col_atts, factorize = factorize)

      # get data for the state of interest and make sure there is data

      if(!is.null(df_state) && nrow(df_state) > 0) {
        fname <- brfss_data_path( rw = 'w')

        # brfss.param(extent = ext)

        if(verbose) cat("Going to save :", fname, "\n")

        saveRDS(df_state,file = fname)
      }

    })

    brfss.param(geog = geog_save)

  })

  brfss.param(version = 0)

  invisible()
}


#' Split one State/Territory from Main File
#'
#' @param df_brfss
#' @param geog
#' @param verbose
#'
#' @returns
#' @export
#'
split_geog<-function(df_brfss, geog = NULL, col_atts = TRUE, factorize = TRUE, verbose=TRUE) {

  # make sure some version will be split

  if(is.null(geog)) return(NULL)

  version <- brfss.param(version)

  brfss.param(geog_flag = 'on')

  if(is.character(geog)) {
    geog_id <- brfss::geogs %>% filter(Abbrev == {{geog}}) %>% pull(Id)
    geog_abb <- geog
  } else if (is.numeric(geog)) {
    geog_abb <- brfss::geogs %>% filter(Abbrev == {{geog}}) %>% pull(Abbrev)
    geog_id <- geog

  } else return(NULL)

  brfss.param(geog = geog_abb)

  add_cols<-character(0)

  geog_type <- df_brfss %>% pull(`_STATE`) %>% class()

  # get data for the state of interest and make sure there is data

  if(geog_type == "numeric") {
    df_state <- df_brfss %>% filter(`_STATE` == {{geog_id}})
  }  else {

    df_state <- df_brfss %>% filter(`_STATE` == {{geog_abb}})
  }

  if(nrow(df_state)>0) {
    if(verbose) cat("Saving ",geog_abb,"V",version,"\n")

    fname <- brfss_data_path( rw = 'w')

    if(col_atts) add_column_attributes()

    if(factorize) df_state <- df_state %>% make_factors(verbose = verbose)
  } else {
    df_state <- NULL
  }

  return(df_state)
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

