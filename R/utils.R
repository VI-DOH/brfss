
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

brfss_geog_data<-function(year,geog,version=0) {

  if(is.numeric(geog)) geog<-geog_abbs(geog)
  if(version>0) {
    fil_name<-apply.pattern("brfss_geog_file_version", YEAR = year, GEOG = geog, VERS=version)
  } else {
    fil_name<-apply.pattern("brfss_geog_file", YEAR = year, GEOG = geog, VERS=version)
  }

  fldr_name<-apply.pattern("brfss_geog_folder", YEAR = year, GEOG = geog, VERS=version)
  fname<-paste0(fldr_name,fil_name)

  if(file.exists(fname)) {
    df_brfss<- orrr::get.rdata(fname)
  } else {
    df_brfss<-NULL
  }

  df_brfss
}

