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
#' Download the annual ASCII BRFSS data file
#'
#'
#' @param year integer - year of data file
#' @param destpath character - location to store file - will be created if it does not exist - default is the data_raw folder in the project root folder
#' @param unzip logical - unzip file?
#' @param rmzip logical - remove the zip file?
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ascii.download.data(2016)
#' }
#'
ascii.download.data<-function(year,destpath = NULL, unzip=TRUE, rmzip=TRUE) {

  if(is.null(destpath)) destpath <- apply.pattern("ascii_raw_data_folder", YEAR = year)
  destpath <- normalizePath(destpath,winslash = "/",mustWork = FALSE)
#
#   if(!grepl("/$",destpath)) destpath <- paste0(destpath,"/")
#
#   destpath<-paste0(destpath,year,"/ascii/")

  if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)

  url<-apply.pattern("ascii_downloads",YEAR =year)
  destfile<-paste0(destpath,"LLCP",year,"ASC.zip")

  browser()
  download.file(url = url ,destfile = destfile)

  if(unzip) {
    exdir<-gsub("/$","",destpath)
    unzip(destfile,exdir = exdir)
    if(rmzip) file.remove(destfile)
  }
}
