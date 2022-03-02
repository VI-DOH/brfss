#https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018ASC.zip

#' URL of BRFSS ASCII data
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
ascii_data_url<-function(year) {
  paste0("https://www.cdc.gov/brfss/annual_data/", year,"/files/LLCP",year,"ASC.zip")
}
#

#' Download ASCII BREFSS data
#'
#' @param year
#' @param destpath
#' @param unzip
#' @param rmzip
#'
#' @return
#' @export
#'
#' @examples
#' download_ascii_data(2016)
#'
download_ascii_data<-function(year,destpath = "./data_raw/", unzip=TRUE, rmzip=TRUE) {

  destpath<-paste0(destpath,year,"/ascii/")

  if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)
  url<-ascii_data_url(year)
  destfile<-paste0(destpath,"LLCP",year,"ASC.zip")

  download.file(url = url ,destfile = destfile)

  if(unzip) {
    exdir<-gsub("/$","",destpath)
    unzip(destfile,exdir = exdir)
    if(rmzip) file.remove(destfile)
  }
}
