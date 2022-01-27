source(orrr::dir.project(c("R","brfss_paths.R"),slash = F))
source(orrr::dir.project(c("R","utils.R"),slash = F))


#' Get Highest Version of Survey for a Year
#'
#'   Looks through the filenames in the BRFSS data folder for files with the pattern  *_Vn.* where n is the version number.
#'
#' @param year - the 4-digit year of interest
#'
#' @return integer - the highest version number found in the filenames
#' @export
#'
#' @examples
#' maxvers<-highest_version(2018)
#'
#'
highest_version<-function(year) {
  fldr<-brfss_data_folder(year)

  files<-list.files(fldr)
  files<-files[grep("_V[0-9][.]",files)]
  vers<-as.integer(gsub(".*_V([0-9]*)[.].*","\\1",files))
  max(vers)
}

calc_responses<-function(year,states,versions) {

  if(missing(versions)) versions=0:highest_version(year)

  if(missing(states)) {
    states<-state_abbs()
  } else {

    if(is.numeric(states)) states<-state_abbs(states)
  }

  df0<-data.frame()
  invisible(
    sapply(states,function(state){
      sapply(versions,function(version){
        if(brfss_state_version_exists(year,state,version)) {

          df0<<-rbind(df0,data.frame(year=year,state=state,version=version,responses=nrow(brfss_state_data(year,state,version))))
        }
      })
    } )
  )
  df0
}

responses_by_state<-function(year,state,version=0) {

  if(is.numeric(state)) state<-state_abbs(state)

  if(brfss_state_version_exists(year,state,version)) {

    df0<-brfss_state_data(year,state,version)

    return(nrow(df0))
  } else {
    return(0)
  }
}


save_response_stats<-function(year) {
  require(dplyr)

  df_responses<-calc_responses(year = year)

  nm<-paste0("df_responses_",year)
  assign(nm,df_responses)
  save(list = c(nm),file = paste0(brfss_data_folder(year = year),"responses_",year,".RData"))

}


responses<-function(year,states,versions, reduce=TRUE) {

  df<- orrr::get.rdata(orrr::dir.project(c("data",year,paste0("responses_",year,".RData")),slash = F))

  if(!missing(states)) {
    if(is.numeric(states)) states<-state_abbs(states)
    df<-df[df$state%in%states,]
  } else {
    states<-state_abbs()
  }

  if(!missing(versions)) {
    df<-df[df$version%in%versions,]
  } else {
    versions<-integer(2)
  }

  if(reduce && length(states)==1 && length(versions)==1) {
    return(df$responses)
  } else {
    return(df)
  }
}


