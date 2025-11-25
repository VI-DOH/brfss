
#' Survey Statistics
#'
#' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' optional weighting.
#'
#' Retuns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#' #'
#'
#' @param df - data.frame: the survey data
#' @param year - integer: if missing will calculate from df
#' @param column - character: column name of interest
#' @param num_vals - integer: values from column in the numerator
#' @param den_vals - integer: values from column in the denominator
#' @param subset - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighting - character: name of column with weighting
#'
#' @return data.frame: statistics for each subset
#' @export
#'
#' @examples
#'
#'
survey_stats<-function(df0,year,column, num_vals,den_vals,subset,conf=.95, weighting) {

  require(package = "survey", quietly = T, warn.conflicts = F)

  coi<-column


  if(missing(year)) year<-df0$IYEAR[1]

  if(nrow(df0)==0) {
    ret<-data.frame()
    return (ret)
  }

  all_cols<-unique(df0[coi])
  all_cols<-as.integer(all_cols[,1])
  #  inc_cols<-unique(c(num_vals,den_vals))
  rem_vals<-all_cols[!(all_cols%in%den_vals)]
  not_num_vals<-all_cols[!(all_cols%in%num_vals)]
  #   -
  yn_rem<-sapply(df0[coi],function(x,y=rem_vals) x%in%y)
  df0[yn_rem,coi]<-NA

  yn_num<-sapply(df0[coi],function(x,y=num_vals) x%in%y)
  yn_not_num<-sapply(df0[coi],function(x,y=not_num_vals) x%in%y)

  df0[yn_num,coi]<-1
  df0[yn_not_num,coi]<-2
  df0<-df0[!is.na(df0[coi]),]
  df0[coi]<- factor(df0[[coi]],levels = c(1,2),
                    labels = c("Yes", "No"))

  #

  ##########################
  ##
  ##  survey package
  ##
  ##
  if (is.na(subset[1])) frmla<- reformulate(c(coi)) else  frmla<- reformulate(c(coi,subset))

  if(!missing(weighting)) weights=reformulate(weighting) else weights=NULL

    des<-svydesign(ids = ~1,
                 strata = NULL,
                 variables =  frmla,
                 data = df0,
                 weights = weights,
                 deff=F)

  if (missing()) {
    mysvymean<-svymean(frmla,des,na.rm = T,deff = F)
    mysvytotal<-svytotal(frmla,des,na.rm = T,deff = F)
    mysvycounts<-unwtd.count(frmla,des,na.rm = T,deff = F)
    nums<-mysvytotal[1]
    dens<- mysvytotal[1] + mysvytotal[2]
    n<-mysvycounts[1]
    nsubs<-0

  } else {
    nsubs<-length(subset)
    mysvymean<-svyby(reformulate(coi),reformulate(subset),des,svymean)
    mysvytotal<-svyby(reformulate(coi),reformulate(subset),des,svytotal)
    mysvycounts<-svyby(reformulate(coi),reformulate(subset),des,unwtd.count)
    nums<-mysvytotal[,nsubs+1]
    dens<- mysvytotal[,nsubs+1] + mysvytotal[,nsubs+2]
    n<-mysvycounts[,nsubs+1]
  }
  #   mysvystat<-svyby(reformulate(coi),reformulate(subset),des,unwtd.count)

  sd<-qnorm(1-(1-conf)/2)

  df<-as.data.frame(mysvymean)
  if (nsubs==0) {
    ncol<-ncol(df)
    df<-df[1,]
  } else {

    df[4+nsubs]<-NULL
    df[2+nsubs]<-NULL
  }
  colnames(df)[1+nsubs]<-"mean"
  colnames(df)[2+nsubs]<-"SE"

  rownames(df)<-NULL

  df$conf<-conf
  df$CI_lower<-df$mean-df$SE*sd
  df$CI_upper<-df$mean+df$SE*sd

  df$CI_lower[df$CI_lower<0]<-0
  df$CI_upper[df$CI_upper>1]<-1

  df$year<-year
##  df$measure<-measure
  df$mcolumn<-coi

  if(nsubs>0) {
    colnames(df)[1:nsubs]<-sapply(1:nsubs,function(x) paste("subval",x,sep="_"))
    sapply(1:nsubs,function(x){
      df[paste("subset",x,sep="_")]<<-subset[x]
    })
  }

  yr_col<-which(colnames(df)=="year")
  ncol<-ncol(df)

  df<-cbind(df[yr_col:ncol],df[-c(yr_col:ncol)])


  mean_col<-which(colnames(df)=="mean")
  cbind(df[1:(mean_col-1)],data.frame(n=n,nums=nums,dens=dens,row.names = NULL),
        df[mean_col:ncol])

}


whichSubvals<-function(df,subvals) {
  cols<-grep("subval",colnames(df),value = T)
  nsubvals_exp<-length(cols)
  nsubvals<-length(subvals)
  if(nsubvals != nsubvals_exp) return(rep(F,nrow(df)))

  yn<-rep(T,nrow(df))

  if(nsubvals_exp>0) {
    dummy<-mapply(function(col,val) {
      yn<<-yn & df[col]==val
    },cols,subvals)
  }

  yn
}
