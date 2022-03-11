
pattern.file.name <- function() {
  normalizePath("./data/file_patterns.RData")
}


init.patterns <- function(){

  x<-c(
    "brfss_url_files","https://www.cdc.gov/brfss/annual_data/[YEAR]/files/",
    "brfss_url_documentation", "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/",
    "brfss_raw_data_folder","./data_raw/",
    "brfss_annual_raw_data_folder","./data_raw/[YEAR]/",
    "brfss_data_folder","./data/",
    "brfss_annual_data_folder","{brfss_data_folder}[YEAR]/",
    "brfss_state_folder","{brfss_data_folder}[YEAR]/geog/",
    "brfss_state_file","[ABB]_[YEAR].RData",
    "brfss_state_path","{brfss_state_folder}{brfss_state_file}",
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
    "sas_rdata","xpt_[YEAR].RData",
    "sas_sasout_version", "SASOUT[YR]_LLCP_V[VERS].SAS",
    "sas_sasout", "SASOUT[YR]_LLCP.SAS",
    "sas_sasout_path", "{sas_folder_raw}{sas_sasout}",
    "sas_file_format", "FORMAT[YR].sas",
    "sas_file_formas", "FORMAS[YR].sas"
  )
  name <- x[seq(1,length(x),2)]
  pattern <- x[seq(2,length(x),2)]

  df_patterns <- data.frame(name , pattern )

  save.patterns(df_patterns)

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

  orrr::get.rdata(pattern.file.name())

}

save.patterns <- function(df_patterns) {

  save(df_patterns,file = pattern.file.name())

}


get.pattern <- function(name_in) {

  df<-orrr::get.rdata(pattern.file.name())
  df<-df[df$name==name_in,]
  #df <-  filter(df,name == {{name_in}})
  pull(df,pattern)

}

apply.pattern <- function(name, year, version="", abb = "") {
  require(dplyr,quietly = T, warn.conflicts = F)

  pats <- get.patterns() %>%
    filter(name == {{name}}) %>%
    pull(pattern)
  #
  pats <- sapply(pats, function(pat) {
    #pat <- get.pattern(name)
    ctr <- 0
    while(grepl("\\{", pat) && ctr<10) {
      pat0 <- gsub(".*?\\{(.*?)\\}.*","\\1",pat)
      pat1<- get.pattern(pat0)
      pat <- gsub(paste0("(\\{",pat0,"\\})"),pat1,pat)
      ctr <- ctr + 1

    }
    pat
  })
  pats <- unname(pats)

  patternize(pats,year, version, abb )

}

################################
patternize<-function(strIn, year, version="", abb = "") {

  ret<-gsub("[YEAR]",year,strIn,fixed = T)
  ret<-gsub("[YR]",year%%100,ret,fixed = T)
  ret<-gsub("[ABB]",abb,ret,fixed = T)
  gsub("\\[VERS\\]",version,ret)
}
