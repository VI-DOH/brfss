library(survey)



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
  cbind(df[1:(mean_col-1)],data.frame(n=n,nums=nums,dens=dens,row.names = NULL),df[mean_col:ncol])
  #  cbind(df[1:(mean_col-1)],data.frame(nums=nums,dens=dens,row.names = NULL),data.frame(yes=num_pos,no=num_neg,n=n,row.names = NULL),df[mean_col:ncol])

}

survey_data_subsets<-function(years,measure="Diabetes",subset=NA,conf=.95,weighted=T) {

  df<-data.frame()

  lst<-  sapply(years, function(yr) {

    df1<-survey_data_subset(yr,measure = measure,subset=subset,weighted = weighted)
    if(nrow(df1)>0) df<<-rbind(df,df1)
    NULL
  })

  df
}


#' Survey Statistics
#'
#' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' optional weighting.
#'
#' Returns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#'
#' @param df0 - data.frame: the survey data
#' @param year - integer: if missing will calculate from df
#' @param coi - character: column name of interest
#' @param ignore - integer: values to ignore in calculating means (usu. for 'UNKNOWN/REFUSED' and/or 'MISSING')
#' @param subset - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighting - character: name of column with weighting
#'
#' @return data.frame: statistics for each subset
#' @export
#'
#' @examples
#'
survey_stats_non_binary<-function(df0, year,coi, ignore,  subset,conf=.95, weighting) {

  require(package = "survey", quietly = T, warn.conflicts = F)


  if(missing(year)) year<-df0$IYEAR[1]

  if(nrow(df0)==0) {
    ret<-data.frame()
    return (ret)
  }

  cols<-coi
  if(!missing(subset)) cols<-c(cols,subset)
  if(!missing(weighting)) cols<-c(cols,weighting)
  df0<-df0[,cols]
  if(!missing(ignore)) df0<-df0[!df0[[coi]]%in%ignore,]

  all_vals<-as.vector(unique(df0[coi]))

  df0$fcoi<-df0[,coi]
  # df0$fcoi<-sample(4,nrow(df0),replace=T)
  df0$fcoi<- factor(df0$fcoi)

  #

  ##########################
  ##
  ##  survey package
  ##
  ##
  if (missing(subset)) frmla<- reformulate(c("fcoi")) else  frmla<- reformulate(c("fcoi",subset))

  if(!missing(weighting)) weights<-reformulate(weighting) else weights=NULL

  #  ids<- reformulate(all_vals)

  des<-svydesign(ids = ~1,
                 strata = NULL,
                 variables =  frmla,
                 data = df0,
                 weights = weights,
                 deff=F)

  if (missing(subset)) {
    mysvymean<-svymean(frmla,des,na.rm = T,deff = F)
    mysvytotal<-svytotal(frmla,des,na.rm = T,deff = F)

    nums<-mysvytotal[1]
    dens<- mysvytotal[1] + mysvytotal[2]
    tot<-nrow(df0)
    nsubs<-0

    df_mean<-t(as.data.frame(mysvymean))
    df_se<-t(as.data.frame(df_mean[2,]))

    colnames(df_se)<-paste("se.fcoi",1:ncol(df_se),sep="")
    df_stats<-cbind(t(as.data.frame(df_mean[1,])),df_se)
    rownames(df_stats)<-"1"
  } else {
    nsubs<-length(subset)
    frmla1<- reformulate(c("fcoi"))
    frmla2<- reformulate(c(subset))

    mysvymean<-svyby(frmla1,frmla2,des,svymean)
    mysvytotal<-svyby(frmla1,frmla2,des,svytotal)
    mysvycounts<-svyby(frmla1,frmla2,des,unwtd.count)
    nums<-mysvytotal[,nsubs+1]
    dens<- mysvytotal[,nsubs+1] + mysvytotal[,nsubs+2]
    tot<-mysvycounts[,nsubs+1]

    df_stats<-data.frame()
    df_stats<-as.data.frame(mysvymean)
  }
  #   mysvystat<-svyby(reformulate(coi),reformulate(subset),des,unwtd.count)
  df_stats<-as.data.frame(df_stats)

  mean_cols<-grep("^fcoi",colnames(df_stats),value = T)
  se_cols<-grep("^se[.]",colnames(df_stats),value = T)
  sub_cols<-colnames(df_stats)[!colnames(df_stats)%in%(c(mean_cols,se_cols))]
  vals<-gsub("fcoi","",mean_cols)

  if(nsubs>0)  df_subs<-df_stats[1:nsubs] else df_subs<-data.frame()
  df_new<-data.frame()
  mapply(function(cmn,cse,val) {
    df_add<-df_stats[,c(sub_cols,cmn,cse)]
    df_add$var<-val

    colnames(df_add)<-c(sub_cols,"mean","se",coi)
    df_new<<-rbind(df_new,df_add)
  }, mean_cols,se_cols,vals)

  df_new<-df_new[,c(ncol(df_new),1:ncol(df_new)-1)]
  sd<-qnorm(1-(1-conf)/2)

  df_new$conf<-conf
  df_new$CI_lower<-df_new$mean-df_new$se*sd
  df_new$CI_upper<-df_new$mean+df_new$se*sd

  df_new$CI_lower[df_new$CI_lower<0]<-0
  df_new$CI_upper[df_new$CI_upper>1]<-1

  df_new$total<-tot
  df_new$year<-year
  ##  df$measure<-measure
  df_new$measure<-coi
  #####################################################
  ##
  ##  add nums
  ##


  if(missing(subset)) {
    num<-sapply(df_new[,coi],  function(c) {
      length(which(df0[,coi]==c))
    })

  } else {
    num<-integer(nrow(df_new))
    for(r in 1:nrow(df_new)){
      yn<-df0[,coi]==df_new[r,coi]
      for(sub in subset) {
        yn<-yn & (df0[,sub]==df_new[r,sub])
      }
      num[r]<-(length(which(yn)))
    }
  }
  df_new$num<-num
  ##
  #
  # yr_col<-which(colnames(df)=="year")
  # ncol<-ncol(df)
  #
  # df<-cbind(df[yr_col:ncol],df[-c(yr_col:ncol)])
  #
  #
  # mean_col<-which(colnames(df)=="mean")
  # cbind(df[1:(mean_col-1)],data.frame(total=tot,nums=nums,dens=dens,row.names = NULL),df[mean_col:ncol])
  #  cbind(df[1:(mean_col-1)],data.frame(nums=nums,dens=dens,row.names = NULL),data.frame(yes=num_pos,no=num_neg,n=n,row.names = NULL),df[mean_col:ncol])
  df_new[,c("year","measure",colnames(df_new)[1:(1+nsubs)],"total","num","mean","se","conf","CI_lower","CI_upper")]
}


survey_data_single<-function(years,measure = "Diabetes",subset=NA,subvals=NA,wtd=T) {


  if(length(subset)!=length(subvals)) return (NA)

  df<-survey_data_subsets(years,measure,subset,subvals,wtd)

  yn<-rep(T,nrow(df))
  cols<-grep("subval",colnames(df),value = T)

  if(length(cols)>0) {
    dummy<-mapply(function(col,val) {
      yn<<-yn & df[col]==val
    },cols,subvals)
  }
  #  if(length(which(!is.na(yn)))==0)
  df[yn,]


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
