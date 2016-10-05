
#' Title
#'
#' @param df - data.frame
#' @param coi - character
#' @param subsets - character
#' @param min_num - integer
#' @param percent - logical
#' @param df_fields - data.frame
#'
#' @return - data.frame
#' @export
#'
#' @examples
#'
prep_stats<-function(df,coi,subsets,min_num=0,percent=T, df_fields) {
  if(missing(df_fields)) load(file = "./data/fields.RData",envir = environment())

  if(!missing(subsets)){
    if(is.null(subsets)) {
      nsubs<-0
    } else {
      nsubs<-length(subsets)
    }
  } else {
    nsubs<-0
    subsets<-c()
  }

  if(percent) {
    df$mean=round(df$mean*100,1)
    df$se=round(df$se*100,3)
    df$CI_lower=round(df$CI_lower*100,1)
    df$CI_upper=round(df$CI_upper*100,1)
  }

  fctr<-df_fields[df_fields$VarName==coi,]

  fctr$Desc<-trimws(fctr$Desc)

  if(nrow(fctr)>0) {
    df[,coi]<-factor(x=df[,coi],levels=c(-2,-1,fctr$Value),labels = c("No ", "Yes ",fctr$Desc))
  }

  if(nsubs>0) {
    sapply(subsets,function(sub) {

      fctr<-df_fields[df_fields$VarName==sub,]
      if(nrow(fctr)>0) {
        df[,sub]<<-factor(x=df[,sub],levels=fctr$Value,labels = fctr$Desc)
      }
    })
  }

  df<-df[df$total>=min_num,]

  #print(svy_stats[svy_stats$SEX=="Female",])
  df$ci<-as.character(mapply(function(l,u) sprintf("%0.1f-%0.1f",l,u),df$CI_lower,df$CI_upper))
  coivals<-unique(df[,coi])
  df[,c(coi, subsets, "total","num", "mean" , "ci")]


}


#' Title
#'
#' @param df - data.frame
#' @param coi - character
#' @param subsets - character
#' @param min_num - integer
#' @param percent - logical
#' @param cat - logical
#' @param invisible - logical
#' @param ... - other parameters
#'
#' @return - data.frame
#' @export
#'
#' @examples
#'
vert_stats_simple<-function(df,coi,subsets,min_num=0,percent=T, cat=FALSE,invisible=FALSE, ... ) {
  df<-prep_stats(df=df,coi=coi,subsets=subsets,min_num,percent, ...)
  if(cat) print(df,row.names=F)
  if(invisible) return(invisible()) else return(df)

}

#' Title
#'
#' @param df - data.frame
#' @param coi - character
#' @param subsets - character
#' @param min_num - integer
#' @param percent - logical
#' @param cat - logical
#' @param invisible - logical
#' @param header - logical
#' @param linesep - character
#' @param headsep - character
#' @param ... - other parameters
#'
#' @return - character
#' @export
#'
#' @examples
#'
horz_stats_simple<-function(df,coi,subsets,min_num=0,percent=T, header=T,linesep,headsep, cat=F,invisible=F, ...) {   #order, ...) {

  fmt_grp_sz<-20
  fmt_total_sz<-5
  fmt_num_sz<-5
  fmt_pct_dig<-1
  fmt_pct_sz<-4+fmt_pct_dig
  fmt_ci_sz<-10
  fmt_spc_btw<-2

  fmt_grp<-paste("%",fmt_grp_sz,"s",sep="")

  if(missing(subsets)) subsets<-c()

  df<-prep_stats(df=df,coi=coi,subsets=subsets,min_num,percent, ...)

  # if(!missing(order)) {
  #     df<-df[order(df$mean,decreasing = (order=="dec")),]
  # }
  #
  if(is.null(subsets)) {
    subsets<-c("DUMMY")
    df$DUMMY<-"Total"
  }

  maxchar<-0

  hdr<-character()
  ret<-character()

  subs<-unique(df[[subsets[1]]])
  cols<- unique(as.character(df[[coi]]))

  #############################################
  ##
  ##  each item of a subset is a new line
  ##
  sapply(subs,function(sub) {
    hdr<<-sprintf(fmt_grp,center_string("GROUP",fmt_grp_sz))
    linex<<-sprintf(fmt_grp,center_string(sub,fmt_grp_sz))

    #############################################
    ##
    ##  each item of the coi is a set of columns
    ##
    col1<-TRUE

    sapply(cols,function(colx) {

      df_line<-df[df[[coi]]==colx & df[[subsets[1]]]==sub,]

      if(col1) {
        hdr<<-paste(hdr,sprintf(" %5s ","TOTAL"),sep="")
        linex<<-paste(linex,sprintf(" %5d ",df_line$total),sep="" )
        col1<<-FALSE
      }
      mapply(function(v3,v4,v5) {
        v5_hdr<-center_string("C.I.",fmt_ci_sz)
        v5<-center_string(v5,fmt_ci_sz,ctr_char =  "-")
        hdr_fmt<-paste("%",fmt_num_sz,"s",spaces(fmt_spc_btw),"%",fmt_pct_sz,"s",spaces(fmt_spc_btw),"%",fmt_ci_sz,"s",sep="")
        hdr<<-paste(hdr,sprintf(hdr_fmt," N ","  %  ",v5_hdr),sep="" )

        line_fmt<-paste("%",fmt_num_sz,"d",spaces(fmt_spc_btw),"%",fmt_pct_sz,".",fmt_pct_dig,"f",spaces(fmt_spc_btw),"%",fmt_ci_sz,"s",sep="")

        linex<<-paste(linex,sprintf(line_fmt,v3,v4,v5),sep="" )
      },df_line$num,df_line$mean,df_line$ci)
    })
    maxchar<<-max(maxchar,nchar(linex))
    ret<<-paste(ret,linex,"\n",sep = "")
  })

  if(header) {


    top_line<-spaces(fmt_grp_sz+fmt_total_sz+fmt_spc_btw)
    sapply(df[[coi]],function(c) {
      top_line<<-paste(top_line,center_string(c,fmt_num_sz+fmt_pct_sz+fmt_ci_sz+2*fmt_spc_btw),sep="")
    })

    hdr<-paste(top_line,"\n",hdr,"\n",sep="")
    if(!missing(headsep)) {
      hdr<-paste(hdr,stringr::str_dup(string = headsep,times=maxchar),"\n",sep="")
    }
  } else {
    hdr<-""
  }

  if(missing(linesep)) {
    x<-paste(hdr,ret,sep="")
  } else {
    x<-paste(hdr,ret,stringr::str_dup(string = linesep,times=maxchar),"\n",sep="")
  }
  if(cat) cat(x)
  if(invisible) return(invisible()) else return(x)
}

