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
highest_version<-function() {

  params <- my.brfss.patterns()

  if(brfss.param(source) == "sas") {
    fldr<-apply.pattern("sas_data_folder", params)
  } else {

    fldr <- apply.pattern("brfss_annual_data_folder", params)
  }

  files<-list.files(fldr)

  if(length(files) == 0) {
    if(brfss.param(source) == "sas") {
      fldr<-apply.pattern("sas_raw_data_folder", params)
    } else {
      fldr <- apply.pattern("ascii_path", params)
    }
    files<-list.files(fldr)

  }

  vers <- 0
  if(length(files) > 0) {

    files<-files[grep("V[0-9][.]",files)]
    vers<-as.integer(gsub(".*V([0-9]*)[.].*","\\1",files))
    if(length(vers)==0) vers <- 0
  }

  max(vers)
}

calc_responses <- function() {
  params <- my.brfss.patterns()

  df_resp <- data.frame()

  sapply(0:highest_version(), function(version){

    brfss.param(version = version)
    params <- my.brfss.patterns()

    if(brfss.param(extent) == "national") {
      df_brfss_vers <- brfss_data()

    } else {


      fldr <- apply.pattern("brfss_geog_folder", params)

      geogs <- list.files(fldr)

      df_brfss_vers <- data.frame()

      sapply(geogs, function(geog) {

        df <- brfss_data()

        df_brfss_vers <<- df_brfss_vers %>% bind_rows(df)
      })

    }

    year <- brfss.param(year)
    version <- brfss.param(version)

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
responses_by_geog<-function() {

  params <- my.brfss.patterns()
  geog <- brfss.param(geog)
  file <- apply.pattern("brfss_responses_path",params)

  if(!file.exists(file)) return(NULL)

  readRDS(file = file) %>%
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
save_response_stats<-function(progress = NULL) {
  require(dplyr)

  show_progress(progress,
                message = paste0("Responses ... Saving"))


  params <- my.brfss.patterns()

  df_responses <- calc_responses()

  nm <- apply.pattern("brfss_responses_df", params)
  assign(nm,df_responses)

  file  <-  apply.pattern("brfss_responses_path",params)
  if(!dir.exists(dirname(file))) dir.create(dirname(file))

  saveRDS(df_responses, file = file)


}


responses<-function(versions, reduce=TRUE) {

  params <- my.brfss.patterns()

  year <- brfss.param(year)
  df<- readRDS(apply.pattern("brfss_responses_path",params))


  geogs<-geog_abbs()

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


