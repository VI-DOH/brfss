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

#' Read BRFSS raw ASCII data file
#'
#'  Read BRFSS raw ASCII data file
#'
#' @param year integer year of interest
#' @param filename character name of the raw ascii data file
#' @param layout character name of the file containing the layout or data.frame containing the layout
#' @param state character include data for "all" states (default), "my" state, or "other" states
#' @param completes logical whether or not to include only complete interviews (default=TRUE)
#'
#' @return data frame containing survey data
#'
#' @examples
#' \dontrun{
#' my.brfss(year = 2020, geog = "MT")
#' df<-read.brfss.ascii(year = ,
#' layout="./data_raw/var_layouts/layout_2016")
#'}
#'
#' @export

read.brfss.ascii<-function(year = NULL, filename=NULL,layout = NULL,state=c("all","my","other"),otherstate="*", completes=T) {

  #  env<-get.brffs.env()
  state <- match.arg(state)

  if(is.null(filename)) {
    year<-get.year(year)

    if(!is.null(year)) {
      filename <- apply.pattern("ascii_path", YEAR = year)
    }

  }

  if(is.null(filename) || is.null(layout)) {
    return (NA)

  }
  ################################
  ##
  ##  get the layout data
  ##
  if(class(layout)=="character") {
    df_fields_yy<-read.brfss.layout(layout)
  } else {
    df_fields_yy<-layout
  }

  ############################################################
  ##
  ##  remove col_names of negative width columns
  ##
  widths = as.integer(df_fields_yy$field_size)

  col.names = df_fields_yy$col_name[widths>0]

  ################################
  ##
  ##  read the file based on the layout
  ##
  df<-read.fwf(filename,widths = widths,
               col.names = col.names,
               stringsAsFactors=F)

  ######################################
  ##
  ##  use the value of state to determine which rows to keep
  ##
  if(grepl("^m|^o",state)) {
    yn<-is.na(df$RSState)
    if(grepl("^o",state)) {
      yn<-grepl(otherstate,state)
    }
  } else {
    yn<-rep(T,nrow(df))
  }

  #######################################################
  ##
  ##  if completes only then make sure DISPCODE in completes range (<2000)
  ##
  if (completes) yn<-yn&(df$DISPCODE<2000)

  ##########################################################
  ##
  ##  return the data frame where conditions (yn) are true
  ##
  df[yn,]

}

#' Read BRFSS raw landline data file
#'
#' Read a BRFSS raw data file containing landline data only
#'
#' @param year integer year of the data included in the file
#' @param month integer month of the data included in the file
#' @param index integer index of the raw data file
#' @param ... other arguments to be passed to read.brfss.ascii
#'
#' @return data frame containing landline data
#'
#' @examples
#' \dontrun{
#' df<-read.brfss.landline(2016,2)
#'}
#'
#' @export


read.brfss.landline<-function(year,month,index=1, ...) {

  ll_filename<-brfss.raw.ascii.filename(year=year,month=month,index=index,type = "landline")

  read.brfss.ascii(filename = ll_filename , ...)

}

#' Read BRFSS raw cell phone data file
#'
#' Read a BRFSS raw data file containing cell phone data only
#'
#' @param year integer year of the data included in the file
#' @param month integer month of the data included in the file
#' @param index integer index of the raw data file
#' @param ... other arguments to be passed to read.brfss.ascii
#'
#' @return data frame containing landline data
#'
#' @examples
#' \dontrun{
#' df<-read.brfss.landline(2016,2)
#'}
#'
#' @export


read.brfss.cell<-function(year,month,index=1, ...) {

    cell_filename<-brfss.raw.ascii.filename(year=year,month=month,index=index,type = "cell")

    read.brfss.ascii(filename = cell_filename , ...)

  }

#' Read BRFSS raw landline and cell phone data file
#'
#' Read BRFSS raw data files containing cell phone data only. A range of months may be included.
#'
#' @param year integer year of the data included in the file
#' @param month integer month[s] of the data included in the file
#' @param index integer index of the raw data file
#' @param ... other parameters to pass to called functions
#'
#' @return data frame containing landline data
#'
#' @examples
#' \dontrun{
#' df<-read.brfss.all(2016,2:5) # will read Feb-May data for 2016
#'}
#'
#' @export


read.brfss.all<-function(year,month,index=1,...) {

  df<-data.frame()
  invisible(sapply(month,function(mo) {
    df_cp<-read.brfss.cell(year=year,month=mo,index=index,...)
    df_ll<-read.brfss.landline(year=year,month=mo,index=index,...)

    df<<-rbind(df,df_cp,df_ll)
  }))
  df
}

#' BRFSS data file processing
#'
#' Reads the file containing layout data for a BRFSS raw data file
#'
#' @param year integer year of the data included in the file
#'
#' @return character default path/filename
#'
#'
#' @export
#'
read.brfss.layout<-function(filename,year) {
  if(missing(filename)) filename<-brfss_layout_filename(year)

  df_fields_yy<-read.csv(filename,stringsAsFactors = F)
  df_fields_yy[complete.cases(df_fields_yy),]
}


#' BRFSS data file processing
#'
#' Reads the file containing field values data for a BRFSS raw data file.
#' For example, the question for, "How many different types of cancer have you had? (CNCRDIFF)", the file contains
#' a record for each of the following: \cr \cr
#' CNCRDIFF,1,	Only one \cr
#' CNCRDIFF,2,	Two \cr
#' CNCRDIFF,3,	Three or more \cr
#' CNCRDIFF,7,	Don't know/Not Sure \cr
#' CNCRDIFF,9,	Refused \cr

#'
#' @param year integer year of the data included in the file
#'
#' @return character default path/filename
#'
#'
#' @export
#'
read.brfss.field_values<-function(year) {
  fname<-brfss_field_values_filename(year)
  df<-read.csv(fname,stringsAsFactors = F)
  df[complete.cases(df),]
}

