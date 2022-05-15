


##############################################################################################################
##
##  get_vertical_table
##
##
###############################################################################################################

#' Title
#'
#' @param df data.frame data of interest
#' @param coi character - column of interest
#' @param binary logical - only Yes/No
#' @param num_vals integer: values from column in the numerator
#' @param den_vals integer: values from column in the denominator
#' @param exclude
#' @param weighting
#' @param min_num
#' @param subsets character: names of column to subset data by
#' @param ques
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
get_vertical_table<-function(df,coi, binary, num_vals,den_vals , exclude, weighting ,min_num=0 , subsets ,ques="", ...) {


  if(binary) {

    svy1<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,weighting = weighting)
    svy2<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = subsets[1],weighting = weighting)
  } else {

    svy1<-survey_stats(df=df, coi = coi, exclude=exclude,
                       weighting = weighting)
    svy2<-survey_stats(df=df, coi = coi, subset = subsets[1] ,exclude=exclude,
                       weighting = weighting)
  }


  cat(paste(split_sentence(ques,len = 80)," (",coi,")","\n",sep=""))

  out<-vert_stats_simple(svy1,coi =coi, min_num = min_num,cat = T,invisible = T, ...)

  sapply(subsets,function(subset){

    if(binary) {
      svy3<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = subset,weighting = weighting)
    } else {
      svy3<-survey_stats(df=df, coi = coi, subset = subset ,exclude=exclude,
                         weighting = weighting)
    }
    len<-nrow(svy3)
    out<-vert_stats_simple(svy3,coi =coi,subsets = subset, min_num = min_num,cat = T,invisible = T, ...)
  })

  cat(newlines(2))


  #######################################
  ##
  ##  Do Subsets
  ##

  if(length(subsets)>1){
    sub_by<-subsets[1]
    sub_vals<-unique(df_brfss[,sub_by])

    sapply(sub_vals,function(sub_val){

      svy2m<-svy2[svy2[,sub_by]==sub_val,]
      nshow<-length(which(svy2m$total>min_num))
      if(nshow>0) {
        out<-vert_stats_simple(svy2m,coi =coi,subsets = subsets[1], min_num = min_num,cat = T,invisible = T, ...)

        sapply(subsets[2:length(subsets)],function(subset){

          if(binary) {
            svy3<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = c(sub_by,subset), weighting = weighting)
          } else {
            svy3<-survey_stats(df=df, coi = coi, subset = c(sub_by,subset) ,exclude=exclude,
                               weighting = weighting)
          }
          svy3m<-svy3[svy3[,sub_by]==sub_val,]
          nshow<-length(which(svy3m$total>min_num))
          if(nshow>0) {
            out<-vert_stats_simple(svy3m,coi =coi,subsets = subset, min_num = min_num,cat = T,invisible = T, ...)
          }

        })
        cat(newlines(2))
      }
    })

  }

  invisible()

}


#' Print BRFSS Data in CDC Format
#'
#' Print summary of results for a column using the style reported by CDC in their final report.
#'
#' For example ... column CVDSTRK3
#'
#'  (Ever told) you had a stroke. (CVDSTRK3)
#'                                     Yes                      No
#'         GROUP         TOTAL    N     %       C.I.      N     %       C.I.
#'  ===========================================================================
#'         Total           841    19    2.0   0.8-3.2    822   98.0  96.8-99.2
#'  ...........................................................................
#'          Male           308     8    1.7   0.3-3.0    300   98.3  97.0-99.7
#'         Female          533    11    2.3   0.5-4.1    522   97.7  95.9-99.5
#'  ...........................................................................
#'  White only, non-Hisp    98     1    0.6   0.0-1.7     97   99.4  98.3-100.0
#'  Black only, non-Hisp   558    10    0.9   0.3-1.6    548   99.1  98.4-99.7
#'  Asian only, non-Hisp    13     0    0.0   0.0-0.0     13  100.0  100.0-100.
#'  Multiracial, non-His    12     0    0.0   0.0-0.0     12  100.0  100.0-100.
#'        Hispanic         110     6    6.9   0.8-13.0   104   93.1  87.0-99.2
#'  Don’t know/Not sure/    33     1    2.0   0.0-5.9     32   98.0  94.1-100.0
#'  ...........................................................................
#'         18-24            39     0    0.0   0.0-0.0     39  100.0  100.0-100.
#'         25-34            75     2    3.9   0.0-9.2     73   96.1  90.8-100.0
#'         35-44            64     0    0.0   0.0-0.0     64  100.0  100.0-100.
#'         45-54           137     1    1.1   0.0-3.1    136   98.9  96.9-100.0
#'         55-64           203     6    3.1   0.4-5.8    197   96.9  94.2-99.6
#'         65+             299    10    3.3   1.0-5.6    289   96.7  94.4-99.0
#'  Don’t know/Refused/M    24     0    0.0   0.0-0.0     24  100.0  100.0-100.
#'  ...........................................................................
#'     Less than H.S.      148     7    4.4   0.7-8.0    141   95.6  92.0-99.3
#'      H.S. or GED        264     7    1.2   0.2-2.2    257   98.8  97.8-99.8
#'      Some College       155     0    0.0   0.0-0.0    155  100.0  100.0-100.
#'    College Graduate     253     5    1.7   0.1-3.3    248   98.3  96.7-99.9
#'  Don’t know/Not sure/    21     0    0.0   0.0-0.0     21  100.0  100.0-100.
#'  ...........................................................................
#'   Less than $14,999     136     2    1.6   0.0-4.2    134   98.4  95.8-100.0
#'   $15,000 - $24,999     159     4    2.6   0.0-5.8    155   97.4  94.2-100.0
#'   $25,000 - $34,999     102     4    2.0   0.0-4.2     98   98.0  95.8-100.0
#'   $35,000 - $49,999      96     2    1.1   0.0-2.7     94   98.9  97.3-100.0
#'   $50,000 - $74,999     106     0    0.0   0.0-0.0    106  100.0  100.0-100.
#'        $75,000+         146     2    0.8   0.0-1.8    144   99.2  98.2-100.0
#'  Don’t know/Not sure     35     2    6.7   0.0-16.9    33   93.3  83.1-100.0
#'        Refused           57     3    4.2   0.0-9.5     54   95.8  90.5-100.0
#'  ...........................................................................
#'
#' @param df [data.frame] BRFSS data
#' @param coi [character] column of interest
#' @param binary [logical] is this a simple yes/no
#' @param num_vals [integer] if binary=T, values representing the numerator
#' @param den_vals [integer] if binary=T, values representing the denominator
#' @param exclude [integer] if binary=F, values in the coi to exclude (usually representing [Don't know/Not sure] and [Refused])
#' @param weighting [character] column containing weight value
#' @param min_num [integer] minimun number in denominator for printing
#' @param subsets [character] columns to subset data by
#' @param ques [character] text of the survey question
#' @param ... arguments to pass to other functions
#'
#' @return
#' @export
#'
#' @examples
get_brfss_style_table<-function(df,coi, binary, num_vals,den_vals , exclude, weighting ,min_num=0 , subsets ,ques="", ...) {

  if(binary) {

    svy1<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,weighting = weighting)
    svy2<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = subsets[1],weighting = weighting)
  } else {

    svy1<-survey_stats(df=df, coi = coi, exclude=exclude,
                       weighting = weighting)
    svy2<-survey_stats(df=df, coi = coi, subset = subsets[1] ,exclude=exclude,
                       weighting = weighting)
  }


  cat(paste(split_sentence(ques,len = 80)," (",coi,")","\n",sep=""))

  out<-horz_stats_simple(svy1,coi =coi, min_num = min_num,linesep = ".",headsep="=",cat=T)

  sapply(subsets,function(subset){

    if(binary) {
      svy3<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = subset,weighting = weighting)
    } else {
      svy3<-survey_stats(df=df, coi = coi, subset = subset ,exclude=exclude,
                         weighting = weighting)
    }
    len<-nrow(svy3)
    out<-horz_stats_simple(svy3,coi =coi,subsets = subset, min_num = min_num,linesep = ".",header = F,cat=T)
  })

  cat(newlines(2))


  #######################################
  ##
  ##  Do Subsets
  ##

  if(length(subsets)>1){
    sub_by<-subsets[1]
    sub_vals<-unique(df_brfss[,sub_by])

    sapply(sub_vals,function(sub_val){

      svy2m<-svy2[svy2[,sub_by]==sub_val,]
      nshow<-length(which(svy2m$total>min_num))
      if(nshow>0) {
        out<-horz_stats_simple(svy2m,coi =coi,subsets = sub_by, min_num = min_num,linesep = ".",header = T,headsep="=",cat=T)

        sapply(subsets[2:length(subsets)],function(subset){

          if(binary) {
            svy3<-survey_stats_binary(df=df,coi=coi, num_vals = num_vals,den_vals = den_vals ,subset = c(sub_by,subset), weighting = weighting)
          } else {
            svy3<-survey_stats(df=df, coi = coi, subset = c(sub_by,subset) ,exclude=exclude,
                               weighting = weighting)
          }
          svy3m<-svy3[svy3[,sub_by]==sub_val,]
          nshow<-length(which(svy3m$total>min_num))
          if(nshow>0) {
            out<-horz_stats_simple(svy3m,coi =coi,subsets = subset, min_num = min_num,linesep = ".",header = F,cat=T)
          }

        })
        cat(newlines(2))
      }
    })

  }

  invisible()
}




