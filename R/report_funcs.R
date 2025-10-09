#' BRFSS data file processing
#'
#' Clean the description field
#'
#' @param desc character string to be cleaned
#'
#' @return character cleaned string vector
#'
#'
#' @export

cleanDesc<-function(desc) {

  desc<-gsub("Don.{1,4}t","Don't",desc)
  desc<-gsub("Hodgkin.s","Hodgkin's",desc)
  desc<-gsub("I.ve","I've",desc)
  desc<-gsub("—","-",desc)
  desc<-gsub("’","'",desc)
  desc
}

#' BRFSS data file processing
#'
#' Reads the file containing layout data for a BRFSS raw data file
#'
#' @param df data.frame containing the measure of interest
#' @param meas character column name
#' @param title character
#' @param order logical
#' @param useNA character
#' @param ... parameters to be passed to other functions
#'
#' @return data.frame containing measure data
#'
#'
#' @export

getMeasureReport<-function(df,meas, title="Var1", order=F ,useNA="ifany", ...) {
  


  df_field_vals<-read.brfss.field_values(...)
  df_meas<- df_field_vals[df_field_vals$VarName==meas,]
  df_meas$Desc<-cleanDesc(df_meas$Desc)

  df_data<-as.data.frame(table(df[,meas],useNA = "ifany"),stringsAsFactors = F)

  colnames(df_data)<-c(title,"Count")


  #   df_meas$Desc<-gsub("’","'",df_meas$Desc)
  if (order) df_data<-df_data[order(df_data$Count,decreasing = T),]
  df<-dplyr::right_join(df_meas,df_data,by=c("Value" = title))[,c("Desc","Count")]

  df
}

#' BRFSS data file processing
#'
#' Reads the file containing layout data for a BRFSS raw data file
#'
#' @param df data.frame containing the measure of interest
#' @param meas character column name
#' @param title character
#' @param order logical
#' @param useNA character
#' @param debug logical
#' @param replace character vector
#' @param replace_vals character vector
#' @param format character
#' @param ... parameters to be passed to other functions
#'
#' @return data.frame containing measure data
#' #'
#'
#' @export

getMeasureReportEach<-function(df,meas, alias, title="Desc", order=F,useNA="ifany", debug=F,
                               replace,replace_vals,  format="%d" ,...) {
  

  df_field_vals<-read.brfss.field_values(...)
  if(missing(alias)) {
    df_meas<- df_field_vals[df_field_vals$VarName==meas,]
  } else {
    df_meas<- df_field_vals[df_field_vals$VarName==alias,]

  }
  df_meas$Desc<-cleanDesc(df_meas$Desc)

  vals<-df[,meas]

  if(!missing(replace)) {
    if(length(replace)!=length(replace_vals)) return (NA)
    mapply(function(rep,rep_val) {
      vals[vals==rep]<<-rep_val
    }, replace,replace_vals)
  }

  df_data<-as.data.frame((table(vals)))


  df_data[,1]<-sapply(as.character(df_data[,1]),function(x) {
    label<-which(x==df_meas$Value)
    if(length(label)==1) {
      return (df_meas$Desc[label])
    } else {
      if(length(grep("%s",format))>0) {
        return (x)
      } else {
        return (sprintf(format,as.integer(x)))
      }

    }
  })

  colnames(df_data)<-c(title,"Count")

  df_data
}
#' BRFSS data file processing
#'
#' Reads the file containing layout data for a BRFSS raw data file
#'
#' @param df data.frame containing the measure of interest
#' @param meas character column name
#' @param meas character breaks
#' @param title character
#' @param order logical
#' @param useNA character
#' @param debug logical
#' @param replace character vector
#' @param replace_vals character vector
#' @param format character
#' @param ... parameters to be passed to other functions
#'
#'
#' @export

getMeasureReportBreaks<-function(df,meas, breaks,  title="Desc", order=F,useNA="ifany", debug=F,
                                 replace,replace_vals, ...) {
  

  df_field_vals<-read.brfss.field_values(...)

  df_meas<- df_field_vals[df_field_vals$VarName==meas,]
  df_meas$Desc<-cleanDesc(df_meas$Desc)

  vals<-df[,meas]

  if(!missing(replace)) {
    if(length(replace)!=length(replace_vals)) return (NA)
    mapply(function(rep,rep_val) {
      vals[vals==rep]<<-rep_val
    }, replace,replace_vals)
  }

  labels<-labelFromBreaks(breaks = breaks)
  vals_other<-vals[vals<=breaks[1] | vals>breaks[length(breaks)]]
  df_other<-as.data.frame((table(vals_other)))
  df_other[,1]<-sapply(df_other[,1],function(x) df_meas$Desc[x==df_meas[,"Value"]])

  if(ncol(df_other)==0) {
    df_other<-data.frame(x=character(0),Count=integer(0))
  }
  colnames(df_other)<-c(title,"Count")

  vals_cut<-as.integer(cut(vals,breaks=breaks,labels = labels))
  df_data<-as.data.frame((table(vals_cut)))


  df_data[,1]<-sapply(df_data[,1],function(x) labels[as.integer(as.character(x))])

  colnames(df_data)<-c(title,"Count")

  if (nrow(x = df_other)>0) rbind(df_data,df_other) else df_data
}

