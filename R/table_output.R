


##############################################################################################################
##
##  get_vertical_table
##
##
###############################################################################################################

#' Title
#'
#' @param df
#' @param coi
#' @param binary
#' @param num_vals
#' @param den_vals
#' @param exclude
#' @param weighting
#' @param min_num
#' @param subsets
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


#' Title
#'
#' @param df
#' @param coi
#' @param binary
#' @param num_vals
#' @param den_vals
#' @param exclude
#' @param weighting
#' @param min_num
#' @param subsets
#' @param ques
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
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




