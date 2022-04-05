#'
#' #' Read BRFSS raw landline data file
#' #'
#' #' Read a BRFSS raw data file containing landline data only
#' #'
#' #' @param year integer year of the data included in the file
#' #' @param month integer month of the data included in the file
#' #' @param index integer index of the raw data file
#' #' @param ... other arguments to be passed to read.brfss.ascii
#' #'
#' #' @return data frame containing landline data
#' #'
#' #' @examples
#' #' \dontrun{
#' #' df<-read.brfss.landline(2016,2)
#' #'}
#' #'
#' #' @export
#'
#'
#' read.brfss.landline<-function(year,month,index=1, ...) {
#'
#'   ll_filename<-brfss.raw.ascii.filename(year=year,month=month,index=index,type = "landline")
#'
#'   read.brfss.ascii(filename = ll_filename , ...)
#'
#' }
#'
#' #' Read BRFSS raw cell phone data file
#' #'
#' #' Read a BRFSS raw data file containing cell phone data only
#' #'
#' #' @param year integer year of the data included in the file
#' #' @param month integer month of the data included in the file
#' #' @param index integer index of the raw data file
#' #' @param ... other arguments to be passed to read.brfss.ascii
#' #'
#' #' @return data frame containing landline data
#' #'
#' #' @examples
#' #' \dontrun{
#' #' df<-read.brfss.landline(2016,2)
#' #'}
#' #'
#' #' @export
#'
#'
#' read.brfss.cell<-function(year,month,index=1, ...) {
#'
#'     cell_filename<-brfss.raw.ascii.filename(year=year,month=month,index=index,type = "cell")
#'
#'     read.brfss.ascii(filename = cell_filename , ...)
#'
#'   }
#'
#' #' Read BRFSS raw landline and cell phone data file
#' #'
#' #' Read BRFSS raw data files containing cell phone data only. A range of months may be included.
#' #'
#' #' @param year integer year of the data included in the file
#' #' @param month integer month[s] of the data included in the file
#' #' @param index integer index of the raw data file
#' #' @param ... other parameters to pass to called functions
#' #'
#' #' @return data frame containing landline data
#' #'
#' #' @examples
#' #' \dontrun{
#' #' df<-read.brfss.all(2016,2:5) # will read Feb-May data for 2016
#' #'}
#' #'
#' #' @export
#'
#'
#' read.brfss.all<-function(year,month,index=1,...) {
#'
#'   df<-data.frame()
#'   invisible(sapply(month,function(mo) {
#'     df_cp<-read.brfss.cell(year=year,month=mo,index=index,...)
#'     df_ll<-read.brfss.landline(year=year,month=mo,index=index,...)
#'
#'     df<<-rbind(df,df_cp,df_ll)
#'   }))
#'   df
#' }

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
  if(missing(filename)) filename<-sas_layout_filename(year)

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

