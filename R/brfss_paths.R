#' BRFSS Data File Information
#'
#' Gets information for a filename containing BRFSS raw data
#'
#' @param filename character name of the file, may include the path
#'
#' @return list containing members path, type, year, month, index
#'
#' @examples
#' \dontrun{
#' filename<-"./data_raw/2016/04_APR/CEL_VIAPR161.dat"
#' lst<-brfss_file_info(filename)
#' year<-lst$year
#'}
#' @export


brfss_file_info<-function(filename) {

  lst<-list()
  if(grepl("/",filename)) {
    pattern<-"(.*/)(VI|CEL_VI)([A-Z]{3})([0-9]{2})([0-9]).dat"

    lst$path<-gsub(pattern,"\\1",filename)
    type<-gsub(pattern,"\\2",filename)
    lst$year<-
      as.integer(gsub(pattern,"\\4",filename)) + 2000

    mmm<-gsub(pattern,"\\3",filename)
    lst$month<-which(mmm==toupper(as.character(month.abb)))

    lst$index<-as.integer(gsub(pattern,"\\5",filename))

    lst$type<-ifelse(grepl("CEL",type),"cell","landline")
  } else {
    pattern<-"(VI|CEL_VI)([A-Z]{3})([0-9]{2})([0-9]).dat"

    lst$path<-NA
    type<-gsub(pattern,"\\1",filename)
    lst$year<-
      as.integer(gsub(pattern,"\\3",filename)) + 2000

    mmm<-gsub(pattern,"\\2",filename)
    lst$month<-which(mmm==toupper(as.character(month.abb)))

    lst$index<-as.integer(gsub(pattern,"\\4",filename))

    lst$type<-ifelse(grepl("CEL",type),"cell","landline")

  }
  lst
}

#' BRFSS data file default path
#'
#' Gets default path for a file containing BRFSS raw data
#'
#' @param year integer year of the data included in the file
#' @param month integer month of the data included in the file
#'
#' @return character default path
#'
#' @examples
#' \dontrun{
#' path<-brfss_raw_path(2016,2)
#'}
#'
#' @export
#'
brfss_raw_path<-function(year,month) {

  yy<-format(year%%100,digits=2)
  mm<-sprintf("%02d",month)
  mmm<-toupper(month.abb[month])

  paste("./data_raw/20",yy,"/",mm,"_",mmm,"/",sep="")

}

#' BRFSS data file default name
#'
#' Gets default name for a file containing BRFSS raw data, including the path if requested
#'
#' @param type character  source of the data in the file ... "landline" or "cell"
#' @param year integer year of the data included in the file
#' @param month integer month of the data included in the file
#' @param index integer index of the raw data file
#' @param ynpath logical (default=TRUE) include the default path?
#'
#' @return character default path
#'
#' @examples
#' \dontrun{
#' filename_raw<-brfss_raw_filename("cell",2016,2)
#'}
#'
#' @export
#'
brfss_raw_filename<-function(type=c("landline","cell"),year,month,index=1, ynpath=T) {

  yy<-format(year%%100,digits=2)
  mm<-sprintf("%02d",month)
  mmm<-toupper(month.abb[month])

  path<-ifelse(ynpath,brfss_raw_path(year,month),"")

  ifelse(type=="landline",
         filename<-paste(path,"VI",mmm,yy,index,".dat",sep=""),
         filename<-paste(path,"CEL_VI",mmm,yy,index,".dat",sep="")
  )
  filename
}

#' BRFSS data file processing
#'
#' Gets default path and filename for a file containing layout data for a BRFSS raw data file
#'
#' @param year integer year of the data included in the file
#'
#' @return character default path/filename
#'
#'
#' @export


sas_layout_filename<-function(year) {

  paste("./data_raw/var_layouts/data_fields_",year,".csv",sep="")
}

#' BRFSS data file processing
#'
#' Gets default path and filename for a file containing field values data for a BRFSS raw data file.
#' For example, the question for, "How many different types of cancer have you had? (CNCRDIFF)", the file contains
#' a record for each of the following:
#' CNCRDIFF,1,	Only one
#' CNCRDIFF,2,	Two  r
#' CNCRDIFF,3,	Three or more
#' CNCRDIFF,7,	Don't know/Not Sure
#' CNCRDIFF,9,	Refused

#'
#' @param year integer year of the data included in the file
#'
#' @return character default path/filename
#'
#'
#' @export

brfss_field_values_filename<-function(year) {

  paste("./data_raw/var_layouts/field_values_",year,".csv",sep="")
}


#' Get BRFSS data file path
#'
#' This is used in two ways based on the `rw` variable.
#'
#' 1. rw = 'w': we want to write it ... find the path and create the folder so the file can be saved
#'
#' 2. rw = 'r': we want to read it ... find the path of the file to be read and return NULL if it doesn't exist
#'
#' @param year integer - year of interest
#' @param geog character - geography of interest
#' @param version integer - survey version (0 = main)
#' @param extent character -'local' or 'national'
#' @param source character - data source ('ascii' or 'sas')
#' @param rw character - read/write
#'
#' @return
#'
#' @export

brfss_data_path <- function(rw = c("r","w")) {

  read <- rw == 'r'
  write <- rw == 'w'

  params <- my.brfss.patterns()

  fldr <- apply.pattern("brfss_annual_data_folder", params)

  if(!dir.exists(fldr) && write) {
    dir.create(fldr, recursive = TRUE)
  }

  file <- apply.pattern("brfss_annual_data_file",  params)

  path <- paste0(fldr,file)

  if(read && !file.exists(path)) path <- NULL

  path
}


brfss_version_exists<-function(version=1) {

  brfss.param(version = version)
  fname<- brfss_data_path(rw = 'r'  )

  ok <- !is.null(fname)

  ok
}

#
# brfss_data_folder<-function(year) {
#   orrr::dir.project(c("data",year))
#
# }
