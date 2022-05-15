# source(orrr::dir.project(c("R","brfss_paths.R"),slash = F))
# source(orrr::dir.project(c("R","utils.R"),slash = F))
#

#' Get Highest Version of Survey for a Year
#'
#'   Looks through the filenames in the BRFSS data folder for files with the pattern  *_Vn.* where n is the version number.
#'
#' @param year - the 4-digit year of interest
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @return integer - the highest version number found in the filenames
#' @export
#'
#' @examples
#'\dontrun{
#' maxvers<-highest_version(2018)
#'}
#'
highest_version<-function(year=NULL, ...) {

  source <- get.source()

  if(is.null(source)) source <- "sas"

  year <- get.year(year)

  if(source == "sas") {
    fldr<-apply.pattern("sas_data_folder",YEAR = year, ...)
  } else {

    fldr <- apply.pattern("ascii_data_folder", YEAR = year, ...)
  }

  files<-list.files(fldr)

  if(length(files) == 0) {
    if(source == "sas") {
      fldr<-apply.pattern("sas_data_folder",YEAR = year, ...)
    } else {
      fldr <- apply.pattern("ascii_path", YEAR = year, ...)
    }
    files<-list.files(fldr)

  }

  vers <- 0
  if(length(files) > 0) {

    files<-files[grep("_V[0-9][.]",files)]
    vers<-as.integer(gsub(".*_V([0-9]*)[.].*","\\1",files))
    if(length(vers)==0) vers <- 0
  }

  max(vers)
}

calc_responses<-function(year,geogs,versions, verbose = FALSE, ...) {

  if(missing(versions)) versions <- 0:highest_version(year)

  if(missing(geogs)) {
    geogs<-geog_abbs()
  } else {

    if(is.numeric(geogs)) geogs<-geog_abbs(geogs)
  }

  df0<-data.frame()
  invisible(
    sapply(geogs,function(geog){
      sapply(versions,function(version){
        if(verbose) cat(paste0(" versions ... trying ", geog, "_V",version,"\n"))
        if(brfss_version_exists(year,geog,version)) {
          browser()
          df_resp_cnts <- brfss_data(year,geog,version)
          df_add <- data.frame(year = year,geog = geog, version = version,
                               responses= nrow(df_resp_cnts))
          df0<<-rbind(df0, df_add)

        }
      })
    } )
  )
  df0
}
calc_responses_SAVE<-function(year,geogs,versions, ...) {

  if(missing(versions)) versions <- 0:highest_version(year)

  if(missing(geogs)) {
    geogs<-geog_abbs()
  } else {

    if(is.numeric(geogs)) geogs<-geog_abbs(geogs)
  }

  df0<-data.frame()
  invisible(
    sapply(geogs,function(geog){
      sapply(versions,function(version){
        browser()
        if(brfss_version_exists(year,geog,version)) {

          df_resp_cnts <- brfss_data(year,geog,version)
          df_add <- data.frame(year = year,geog = geog, version = version,
                               responses= nrow(df_resp_cnts))
          df0<<-rbind(df0, df_add)

        }
      })
    } )
  )
  df0
}

responses_by_geog<-function(year,geog,version=0) {

  if(is.numeric(geog)) geog<-geog_abbs(geog)

  if(brfss_version_exists(year,geog,version)) {

    df0<-brfss_data(year,geog,version)

    return(nrow(df0))
  } else {
    return(0)
  }
}


#' Save Response Statistics
#'
#' Calculate the number of responses for each version by geography
#'
#' @param year integer - year of interest
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_response_stats(2018)
#' }
save_response_stats<-function(year, ...) {
  require(dplyr)

  df_responses<-calc_responses(year = year, ...)
  nm<-paste0("df_responses_",year)
  assign(nm,df_responses)


  save(list = c(nm),file = apply.pattern("brfss_responses_path",YEAR = year))

}

responses<-function(year = NULL,geogs=NULL,versions, reduce=TRUE) {

  year <- get.year(year)

  df<- orrr::get.rdata(apply.pattern("brfss_responses_path",YEAR = year))

  if(!is.null(geogs)) {
    if(is.numeric(geogs)) geogs<-geog_abbs(geogs)
    df<-df[df$geog%in%geogs,]
  } else {
    geogs<-geog_abbs()
  }

  if(!missing(versions)) {
    df<-df[df$version%in%versions,]
  } else {
    versions<-integer(2)
  }

  if(reduce && length(geogs)==1 && length(versions)==1) {
    return(df$responses)
  } else {
    return(df)
  }
}


