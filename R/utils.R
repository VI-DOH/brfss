#' @importFrom magrittr %>%
#'
show_progress <- function(progress, message) {

  if(!is.null(progress)) progress$set(message = message, value = NULL)
}

str_something <- function(string) {

  ok <- !is.na(string) && !is.null(string) && length(string)>0 && nchar(string)>0
  ok
}

geog_ids<-function(geogs) {

  geogs[is.na(geogs)] <- ""

  ##  get data.frame of geogs

  df_geogs<- readRDS(paste0(orrr::dir.project("data"),"geogs.rds"))

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

  df_geogs<- readRDS(paste0(orrr::dir.project("data"),"geogs.rds"))

  if(missing(geogs)) {
    geogs<-df_geogs$Abbrev
  } else {
    geogs[is.na(geogs)] <- ""

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
  unlist(geogs)
}


geog_names<-function(geogs) {

  geogs[is.na(geogs)] <- ""

  ##  get data.frame of geogs

  df_geogs<- readRDS(paste0(orrr::dir.project("data"),"geogs.rds"))

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

brfss_web_files <- function(year) {

  require(rvest)
  require(dplyr)

  myurl <- paste0('https://www.cdc.gov/brfss/annual_data/annual_',year,'.html')

  status <- httr::HEAD(myurl)$status
  if(status==200) {
    page  <- read_html(myurl)
    hrefs <- as.character(html_nodes(page, "a") %>% grep("href", ., value = TRUE))

    rm(page)

    hrefs <- hrefs[nchar(hrefs) < 400]

    hrefs <- hrefs %>% grep("pdf",.,value = T)

    url <- gsub(".*href=.*?\"(.*?)\".*","\\1",hrefs)

    txt <- gsub(".*href=.*?\".*?\">(.*?)<.*","\\1",hrefs)

    df <- data.frame(url = url, txt = txt)
  } else {
    df <- data.frame()
  }
  df
}


sex_var <- function(df) {

  cols <- c("Sex", "SEX", "SEXVAR")
  x <- which(cols %in% colnames(df))

  if(!length(x)==0) x <- cols[min(x)]
  as.character(x)
}

pop_sex <- function(df = NULL, coi) {

  if(is.null(df)) df <- prepped_data()

  sexvar <- sex_var(df)

  if(length(sexvar) == 0)  return("Unknown")

  vars <- c(sexvar,coi)

  df %>%
    select(all_of(vars)) %>%
    rename(COI = coi) %>%
    rename(Sex = sexvar) %>%
    filter(!is.na(COI)) %>%
    group_by(Sex) %>%
    count(COI) %>%
    pull(Sex) %>%
    unique() %>%
    as.character() %>%
    {ifelse(length(.)>1,"All",.)}

}
########################################################
##
##    converts raw bytes to a single character for
##     converting strange encodings for " " and "'"
##      that cause problems later on

convert_raw_chars <- function(lines) {
  sp2 <- rawToChar(as.raw(c(0xc2,0xa0)))
  hyph2 <- rawToChar(as.raw(c(0x82,0x09)))
  apost2 <- rawToChar(as.raw(c(0xe2, 0x80, 0x99)))


  lines %>%
    gsub(sp2, " ", .) %>%
    #gsub(hyph2, "-", .) %>%
    gsub(apost2, "'", .)

}
