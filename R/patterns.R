
pattern.file.name <- function() {
  normalizePath("./data/file_patterns.rda", winslash =  "/", mustWork = FALSE)
}


init.patterns <- function(){

  x<-c(
    "data_folder","./data/",
    "raw_data_folder","./data/",
    "brfss_url_files","https://www.cdc.gov/brfss/annual_data/[YEAR]/files/",
    "brfss_url_documentation", "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/",
    "brfss_raw_data_folder","./data_raw/",
    "brfss_annual_raw_data_folder","./data_raw/[YEAR]/",
    "brfss_data_folder","./data/",
    "brfss_annual_data_folder","{brfss_data_folder}[YEAR]/",
    "brfss_geog_folder","{brfss_data_folder}[YEAR]/geog/",
    "brfss_geog_file","[GEOG]_[YEAR].RData",
    "brfss_geog_file_version","[GEOG]_[YEAR]_V[VERS].RData",
    "brfss_geog_path","{brfss_geog_folder}{brfss_geog_file}",
    "brfss_columns_file","{brfss_data_folder}[YEAR]/columns_[YEAR].RData",
    "brfss_layout_folder","{brfss_annual_data_folder}",
    "brfss_layout_file","layout_[YEAR].RData",
    "brfss_layout_path","{brfss_layout_folder}{brfss_layout_file}",
    "ascii_raw_data_folder","{brfss_raw_data_folder}[YEAR]/ascii/",
    "ascii_downloads","{brfss_url_files}LLCP[YEAR]ASC.zip",
    "sas_downloads","LLCP[YEAR]XPT.zip",
    "sas_downloads","SASOUT[YR]_LLCP.SAS",
    "sas_downloads","Format[YR].sas",
    "sas_downloads","Formas[YR].sas",
    "sas_version_downloads","LLCP[YR]V[VERS]_XPT.zip",
    "sas_version_downloads","SASOUT[YR]_LLCP_V[VERS].SAS",
    "sas_folder_raw","{brfss_raw_data_folder}[YEAR]/sas/",
    "sas_folder_data","{brfss_annual_data_folder}",
    "xpt_file", "LLCP[YEAR].XPT",
    "xpt_file_version", "LLCP[YR]V[VERS].XPT",
    "sas_rdata","xpt_[YEAR].RData",
    "sas_rdata_version","xpt_[YEAR]_V[VERS].RData",
    "sas_sasout_version", "SASOUT[YR]_LLCP_V[VERS].SAS",
    "sas_sasout", "SASOUT[YR]_LLCP.SAS",
    "sas_sasout_path", "{sas_folder_raw}{sas_sasout}",
    "sas_file_format", "FORMAT[YR].sas",
    "sas_file_formas", "FORMAS[YR].sas"
  )
  name <- x[seq(1,length(x),2)]
  pattern <- x[seq(2,length(x),2)]

  file_patterns <- data.frame(name , pattern )

  usethis::use_data(file_patterns, overwrite = TRUE)

}

add.pattern <- function(name,pattern) {
  require(dplyr)

  df_patterns <- get.patterns() %>%
    bind_rows(data.frame(name , pattern )) %>%
    distinct()

  save.patterns(df_patterns)

}

remove.patterns <- function(names) {
  require(dplyr)

  df_patterns <- get.patterns() %>%
    filter(!name %in% names) %>%
    distinct()

  save.patterns(df_patterns)

}

pattern.names <- function(filter="*") {
  get.patterns() %>%
    filter(grepl(filter,name)) %>%
    pull(name)
}


get.patterns <- function() {

  file_patterns <- orrr::get.rdata(pattern.file.name())
  if(is.null(file_patterns)) {

    data("file_patterns")
    save.patterns(file_patterns)
  }

  file_patterns
}

save.patterns <- function(file_patterns) {

  save(file_patterns,file = pattern.file.name())

}

expand.pattern <- function(pattern) {

  ctr <- 0
  while(grepl("\\{", pattern) && ctr<10) {
    pat0 <- gsub(".*?\\{(.*?)\\}.*","\\1",pattern)
    pat1<- get.pattern(pat0)
    pattern <- gsub(paste0("(\\{",pat0,"\\})"),pat1,pattern)
    ctr <- ctr + 1

  }
  pattern
}

get.pattern <- function(name_in) {

  df<-orrr::get.rdata(pattern.file.name())
  df<-df[df$name==name_in,]
  #df <-  filter(df,name == {{name_in}})
  dplyr::pull(df,pattern)

}

apply.pattern <- function(name,  ...) {
  require(dplyr,quietly = T, warn.conflicts = F)

  pats <- get.patterns() %>%
    filter(name == {{name}}) %>%
    pull(pattern)
  #
  pats <- sapply(pats, function(pat) {
    #pat <- get.pattern(name)
    pat <- expand.pattern(pat)
    # ctr <- 0
    # while(grepl("\\{", pat) && ctr<10) {
    #   pat0 <- gsub(".*?\\{(.*?)\\}.*","\\1",pat)
    #   pat1<- get.pattern(pat0)
    #   pat <- gsub(paste0("(\\{",pat0,"\\})"),pat1,pat)
    #   ctr <- ctr + 1
    #
    # }
    pat
  })
  pats <- unname(pats)

  patternize(pats,... )

}

################################
patternize_old<-function(strIn, year, version="", geog = "", ...) {

  browser()
  ret<-gsub("[YEAR]",year,strIn,fixed = T)
  ret<-gsub("[YR]",year%%100,ret,fixed = T)
  ret<-gsub("[GEOG]",geog,ret,fixed = T)
  gsub("\\[VERS\\]",version,ret)
}

patternize<-function(strIn, ...) {

  args <- list(...)

  nms <- names(args)
  vals <- unlist(unname(args))

  ###########################################################################
  ##
  ##  this is a special case for YEAR ... creating the 2-digit year as well
  yr_arg <- which(nms == "YEAR")

  if(length(yr_arg)>0) {
    year <- as.integer(vals[yr_arg[1]])
    nms[length(nms)+1] <- "YR"
    vals[length(vals)+1] <- year%%100

  }

  #############################################################################
  ret <- strIn

  mapply(function(nm,val) {
    nm <- paste0("[",nm,"]")
    ret<<-gsub(nm,val,ret,fixed = T)


  }, nms, vals)
  #
  # ret<-gsub("[YEAR]",year,strIn,fixed = T)
  # ret<-gsub("[YR]",year%%100,ret,fixed = T)
  # ret<-gsub("[GEOG]",state,ret,fixed = T)
  # gsub("\\[VERS\\]",version,ret)

  ret
}
