


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

split_sentence<-function(x,len, start=0){
  #
  # find spaces
  #
  lenx<-nchar(x)

  sp_all<-gregexpr(pattern = " ",text = x)

  if(length(sp_all)>0) {

    sp_all<-as.integer(sp_all[[1]])

    # get the spaces after the start of the search (start) and
    # before the length of interest for each line (start + len)
    #
    sp<-sp_all[sp_all<(start+len) & sp_all>start]

    # see if there are any matches
    if(length(sp)>0 && (start+len)<lenx) {

      # insert new line (\n) at the correct space (largest in this group)
      #
      nl<-max(sp)

      #     ptrn<-paste("(.{",nl-1,"}) (.*)",sep="")
      #     x<-gsub(ptrn,"\\1\n\\2",x)
      x<-paste(substring(x,1,nl-1),substring(x,nl+1),sep="\n")
      #
      # see if there are any more
      #
      if(nl<sp_all[length(sp_all)]) {
        x<-split_sentence(x,len,nl)
      }
    }
  }
  x
}

