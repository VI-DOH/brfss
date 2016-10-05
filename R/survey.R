
#' Survey Statistics
#'
#' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' optional weighting. This coerces the values in num_vals (numerator values) to 'Yes' and all values in den_vals (denominator values)
#' not in num_vals to 'No'
#'
#' Retuns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#' #'
#'
#' @param df0 - data.frame: the survey data
#' @param year - integer: if missing will calculate from df
#' @param coi - character: column name of interest
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

survey_stats_binary<-function(df0,coi, num_vals,den_vals, ...) {


  if(nrow(df0)==0) {
    ret<-data.frame()
    return (ret)
  }

  df0<-df0[df0[,coi]%in%den_vals,]


  df0$fcoi<-(-2)
  df0[df0[,coi]%in%num_vals,"fcoi"]<-(-1)
  #  df0[yn_not_num,coi]<-2
  df0<-df0[!is.na(df0[coi]),]
  # df0["fcoi"]<- factor(df0[["fcoi"]],levels = c(-1,-2),
  #                      labels = c("Yes", "No"))
  df0[coi]<-NULL
  colnames(df0)[colnames(df0)=="fcoi"]<-coi

  df<-survey_stats(df0=df0,coi=coi, ...)
  df[order(df[,coi],decreasing = F),]
}

#'
#' Survey Statistics
#'
#' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' optional weighting. This coerces the values in num_vals (numerator values) to 'Yes' and all values in den_vals (denominator values)
#' not in num_vals to 'No'
#'
#' Retuns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#' #'
#'
#' @param df0 - data.frame: the survey data
#' @param coi - character: column of interest name for analysis
#' @param exclude - integer: values in coi to exclude for analysis purposes
#' @param subset - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighting - character: name of column with weighting
##'
#' @return data.frame: statistics for the coi each subset
#' @export
#'
#' @examples
#'
survey_stats<-function(df0, coi, exclude, subset, conf=.95, weighting) {

  require(package = "survey")

  if(!missing(subset)){
    if(is.null(subset)) {
      nsubs<-0
    } else {
      nsubs<-length(subset)
    }
  } else {
    nsubs<-0
    subset<-c()
  }


  #  if(missing(year)) year<-df0$IYEAR[1]

  if(nrow(df0)==0) {
    ret<-data.frame()
    return (ret)
  }

  df0<-df0[!is.na(df0[coi]),]
  cols<-coi
  if(nsubs>0) cols<-c(cols,subset)
  if(!missing(weighting)) cols<-c(cols,weighting)
  df0<-df0[,cols]
  if(!missing(exclude)) df0<-df0[!df0[[coi]]%in%exclude,]

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
  if (nsubs==0) frmla<- reformulate(c("fcoi")) else  frmla<- reformulate(c("fcoi",subset))

  if(!missing(weighting)) weights<-reformulate(weighting) else weights=NULL

  #  ids<- reformulate(all_vals)

  des<-survey::svydesign(ids = ~1,
                 strata = NULL,
                 variables =  frmla,
                 data = df0,
                 weights = weights,
                 deff=F)

  if (nsubs==0) {
    mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)
    mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F)

    nums<-mysvytotal[1]
    dens<- mysvytotal[1] + mysvytotal[2]
    tot<-nrow(df0)

    df_mean<-t(as.data.frame(mysvymean))
    df_se<-t(as.data.frame(df_mean[2,]))

    colnames(df_se)<-paste("se.fcoi",1:ncol(df_se),sep="")
    df_stats<-cbind(t(as.data.frame(df_mean[1,])),df_se)
    rownames(df_stats)<-"1"
  } else {
    frmla1<- reformulate(c("fcoi"))
    frmla2<- reformulate(c(subset))

    mysvymean<-survey::svyby(frmla1,frmla2,des,svymean)
    mysvytotal<-survey::svyby(frmla1,frmla2,des,svytotal)
    mysvycounts<-survey::svyby(frmla1,frmla2,des,unwtd.count)
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
  #  df_new$year<-year
  ##  df$measure<-measure
  df_new$measure<-coi
  #####################################################
  ##
  ##  add nums
  ##


  if(nsubs==0) {
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
  #  df_new[,c("year","measure",colnames(df_new)[1:(1+nsubs)],"total","num","mean","se","conf","CI_lower","CI_upper")]
  df_new[,c("measure",colnames(df_new)[1:(1+nsubs)],"total","num","mean","se","conf","CI_lower","CI_upper")]
}
