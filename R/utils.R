


str_something <- function(string) {

  ok <- !is.na(string) && !is.null(string) && length(string)>0 && nchar(string)>0
  ok
}


geog_ids<-function(geogs) {

  ##  get data.frame of geogs

  df_geogs<- orrr::get.rdata(paste0(orrr::dir.project("data"),"geogs.RData"))

  if(is.character(geogs)) {

    if(nchar(geogs[1])==2) {
      geogs<-sapply(geogs,function(geog) {
        df_geogs[df_geogs$Abbrev==geog,"Id"]
      })
    } else {
      geogs<-sapply(geogs,function(geog) {
        df_geogs[df_geogs$State==geog,"Id"]

      })
    }
  } else {
    return(geogs)
  }
  geogs
}


geog_abbs<-function(geogs) {

  ##  get data.frame of geogs

  df_geogs<- orrr::get.rdata(paste0(orrr::dir.project("data"),"geogs.rda"))

  if(missing(geogs)) {
    geogs<-df_geogs$Abbrev
  } else {
    if(is.character(geogs)) {

      if(nchar(geogs[1])==2) {
        return(geogs)
      } else {
        geogs<-sapply(geogs,function(geog) {
          df_geogs[df_geogs$Geog==geog,"Abbrev"]

        })
      }
    } else {
      geogs<-sapply(geogs,function(geog) {
        df_geogs[df_geogs$Id==geog,"Abbrev"]

      })  }

  }
  geogs
}


geog_names<-function(geogs) {

  ##  get data.frame of geogs

  df_geogs<- orrr::get.rdata(paste0(orrr::dir.project("data"),"geogs.RData"))

  if(is.character(geogs)) {

    if(nchar(geogs[1])!=2) {
      return(geogs)
    } else {
      geogs<-sapply(geogs,function(geog) {
        df_geogs[df_geogs$Abbrev==geog,"State"]

      })
    }
  } else {
    geogs<-sapply(geogs,function(geog) {
      df_geogs[df_geogs$Id==geog,"State"]

    })  }
  geogs
}


