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

calc_responses <- function(year = NULL, extent = "national", source = NULL, ...) {

  year <- get.year(year)
  extent <- get.extent(extent)
  source <- get.source(source)

  df_resp <- data.frame()
  browser()
  sapply(0:highest_version(year), function(version){
    if(extent == "national") {
      df_brfss_vers <- brfss_data(year = year, geog = "*", extent="national", version = version)

    } else {

      fldr <- apply.pattern("brfss_geog_folder", YEAR = year)

      geogs <- list.files(fldr)

      df_brfss_vers <- data.frame()

      sapply(geogs, function(geog) {
        browser()
        df <- brfss_data(year = year, geog = geog, extent="local", version = version)

        df_brfss_vers <<- df_brfss_vers %>% bind_rows(df)
      })

    }

    if(nrow(df_brfss_vers) > 0) {
      df_resp <<- df_resp %>% bind_rows(df_brfss_vers  %>%
                                          group_by(`_STATE`) %>% summarise(n=n()) %>%
                                          mutate(geog = geog_abb(`_STATE`))%>%
                                          mutate(year = {{year}}) %>%
                                          mutate(version = version) %>%
                                          rename(responses = n) %>%
                                          select(year,geog,version,responses) %>%
                                          as.data.frame()
      )
    }


  })
  df_resp
}

#' Response Totals
#'
#' @param year integer: the 4-digit year of interest
#' @param geog character or integer: either the 2-char abbrev, the full name,
#' or the FIPS ID for the geography of interest
#'
#' @return data frame with geog version and count
#' @export
#'
#' @examples
responses_by_geog<-function(year,geog) {

  geog<-geog_abbs(geog)

  orrr::get.rdata(file = apply.pattern("brfss_responses_path",YEAR = year)) %>%
    filter(geog == {{geog}})

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


