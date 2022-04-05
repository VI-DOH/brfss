2
#' Build BRFSS raw data file (ASCII) filename
#'
#' Build BRFSS raw data file filename based on the year, month, index, type, and state
#'
#' @param year integer year of the data included in the file
#' @param month integer month of the data included in the file
#' @param index integer index of the raw data file
#' @param type character either cell or landline
#' @param state character include data for "all" states (default), "my" state, or "other" states
#' @param ynpath logical include path as part of returned file name
#'
#' @return character file name based on passed arguments
#'

#'
#' @export
brfss.raw.ascii.filename<-function(year,month,index=1,type, state="VI", ynpath=F) {

  ##############################################
  ##
  ## argument type can be any no. of chars
  ##  of the choices (e.g. "l" or "la" or "lan" or "land")
  ##
  type<-match.arg(type,c("cell","landline"))

  yy<-format(year%%100,digits=2)
  mm<-sprintf("%02d",month)
  mmm<-toupper(month.abb[month])

  path<-ifelse(ynpath,brfss_raw_path(year,month),"")

  ifelse(type=="landline",
         filename<-paste(path,state,mmm,yy,index,".dat",sep=""),
         filename<-paste(path,"CEL_",state, mmm,yy,index,".dat",sep="")
  )
  filename
}

