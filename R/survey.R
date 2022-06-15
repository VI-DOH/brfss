
#' Get BRFSS Survey Data
#'
#' @param year - int - year of interest
#' @param geog - character - 2-char geog of interest (ex: "NY")
#' @param version - survey version; 0 (default) = main survey
#' @param extent - character - extent of data ("local" or national")
#' @param source - character - source of data ("ascii" or sas")
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- brfss_data(2020, geog = "MD", version = 1)
#'
#' }
#'
brfss_data <- function(year = NULL, geog = NULL, version = 0, extent = NULL, source = NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)
  source <- get.source(source)
  extent <- get.extent(extent)

  if(is.numeric(geog)) geog<-geog_abbs(geog)


  fname <- brfss_data_path(year,geog,version,rw = 'r')

  if(is.null(fname))  return(NULL)

  df_brfss<- orrr::get.rdata(fname)

  df_brfss
}


#' Value Representing Binary No
#'
#' Retrieve the value used to represent 'No' in binary (Yes/No) calculations.
#' When there are more than 2 possible values in the denominator and/or numerator,
#' then it is convenient to convert all values in a numerator to Yes/No
#'
#' @return integer - value representing 'No'
#' @export
#'
binary_no<-function() {
  return(-2)
}

#' Value Representing Binary Yes
#'
#' Retrieve the value used to represent 'Yes' in binary (Yes/No) calculations.
#' When there are more than 2 possible values in the denominator and/or numerator,
#' then it is convenient to convert all values in a numerator to Yes/No
#'
#' @return integer - value representing 'Yes'
#' @export
#'
binary_yes<-function() {
  return(-1)
}

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
#' @param df_brfss - data.frame: the survey data
#' @param year - integer: if missing will calculate from df
#' @param coi - character: column name of interest
#' @param num_vals - integer: values from column in the numerator
#' @param den_vals - integer: values from column in the denominator
#' @param subset - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighting - character: name of column with weighting
#' @param ... other arguments to be passed to other functions
#'
#' @return data.frame: statistics for each subset
#' @export
#'
#' @examples
#'
#'
survey_stats_binary<-function(df_brfss,coi, num_vals,den_vals, ...) {

  #browser()
  if(nrow(df_brfss)==0) {
    ret<-data.frame()
    return (ret)
  }
  df_brfss<-as.data.frame(df_brfss)
  df_brfss<-df_brfss[df_brfss[,coi]%in%den_vals,]


  df_brfss$fcoi<-binary_no()
  df_brfss[df_brfss[,coi]%in%num_vals,"fcoi"]<-binary_yes()
  #  df_brfss[yn_not_num,coi]<-2
  df_brfss<-df_brfss[!is.na(df_brfss[coi]),]
  # df_brfss["fcoi"]<- factor(df_brfss[["fcoi"]],levels = c(-1,-2),
  #                      labels = c("Yes", "No"))
  df_brfss[coi]<-NULL
  colnames(df_brfss)[colnames(df_brfss)=="fcoi"]<-coi

  df<-survey_stats(df_brfss=df_brfss,coi=coi, ...)
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
#' @param df_brfss - data.frame: the survey data
#' @param coi - character: column of interest name for analysis
#' @param exclude - integer: values in coi to exclude for analysis purposes
#' @param subset - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighted - logical: use weighting
##'
#' @return data.frame: statistics for the coi each subset
#' @export
#'
#' @examples
#'
survey_stats<-function(df_brfss = NULL, year = NULL, geog = NULL, extent = NULL, source = NULL,
                       coi, exclude = c("Don.*t|Refuse"), subset = NULL,
                       conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {

  require(survey, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)

  # if data frame not provided then get it from
  if(is.null(df_brfss)) {
    year <- get.year(year)
    geog <- get.geog(geog)
    extent <- get.extent(extent)
    source <- get.source(source)

    df_brfss <- coi_data( coi = coi, subset = subset, year = year, geog= geog,
                          extent = extent, source = source,
                          exclude = exclude)

  }

  if(nrow(df_brfss)==0) {
    ret<-data.frame()
    return (ret)
  }

  if(is.null(subset)) {
    nsubs<-0
  } else {
    nsubs<-length(subset)
  }

  # only the valid values - remove the excludes

  ##########################
  ##
  ##  survey package
  ##
  ##
  cols <- c(coi,subset)  %>% paste0("`",.,"`")
  frmla<- reformulate(cols)

  if(weighted) weighting<-reformulate("FINAL_WT")   else weighting=NULL
  strata<-reformulate("STRATUM")   #else weights=NULL

  #  ids<- reformulate(all_vals)

  options(survey.lonely.psu = "adjust")


  des<-survey::svydesign(ids = ~1,
                         strata = strata,
                         variables =  frmla,
                         data = df_brfss,
                         weights = weighting,
                         deff=F)

  if (nsubs==0) {
    mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)
    mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F)

    nums<-mysvytotal[1]
    dens<- mysvytotal[1] + mysvytotal[2]
    tot<-nrow(df_brfss)

    df_mean<-t(as.data.frame(mysvymean))
    df_se<-t(as.data.frame(df_mean[2,]))

    colnames(df_se)<-paste("se.fcoi",1:ncol(df_se),sep="")
    df_stats<-cbind(t(as.data.frame(df_mean[1,])),df_se)
    rownames(df_stats)<-"1"
  } else {

    frmla1<- reformulate(c(coi) %>% paste0("`",.,"`"))
    frmla2<- reformulate(c(subset) %>% paste0("`",.,"`"))

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

  mean_cols<-grep(paste0("^`{0,1}",coi),colnames(df_stats),value = T)
  se_cols<-grep("^se[.]",colnames(df_stats),value = T)
  sub_cols<-colnames(df_stats)[!colnames(df_stats)%in%(c(mean_cols,se_cols))]
  vals<-gsub(coi,"",mean_cols)

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
  df_new$measure<-coi

  #####################################################
  ##
  ##  add nums
  ##

  if(nsubs==0) {
    num<-sapply(df_new[,coi],  function(c) {
      length(which(df_brfss[,coi]==c))
    })

  } else {
    num<-integer(nrow(df_new))
    for(r in 1:nrow(df_new)){
      yn<-df_brfss[,coi]==df_new[r,coi]
      for(sub in subset) {
        yn<-yn & (df_brfss[,sub]==df_new[r,sub])
      }
      num[r]<-(length(which(yn)))
    }
  }

  df_new %>%
    mutate(num = {{num}})  %>%
    mutate(across(c(mean,starts_with("CI")), ~ .*ifelse(pct,100,1)))  %>%
    mutate(across(c(mean,starts_with("CI")),round,digits))%>%
    select(-se, -conf) %>%
    relocate(total, .before = 1)%>%
    relocate(num, .before = 1)%>%
    relocate(measure, .before = 1) %>%
    mutate({{coi}} := gsub("`","",.[[coi]])) %>%
    relocate(all_of(c(coi,subset)),.after = 1) %>%
    `rownames<-`( NULL )
}

#################################################################################
#################################################################################
##
##      Simple Percents
##

#' Simple Percents and Confidence Intervals
#'
#' Get the percent and CI for a column of data
#'
#' @param df_brfss data.frame: the data frame with the columns nedded
#' @param year integer: year of interest
#' @param geog character: geography of interest
#' @param coi character: column of interest
#' @param wt character: column name of weights
#' @param strata character: column name of strata
#' @param exclude character: pattern for excludes
#'
#' @return Object of class "svystat"
#' @export
#'
#' @examples
simple_stats <- function(df_brfss = NULL, year = NULL, geog = NULL, extent = NULL, source = NULL,
                         coi, subset = NULL,
                         exclude = c("Don.*t|Refuse")) {

  require(dplyr)
  require(magrittr)
  require(survey)
  require(tibble)

  # if data frame not provided then get it from
  if(is.null(df_brfss)) {
    year <- get.year(year)
    geog <- get.geog(geog)
    extent <- get.extent(extent)
    source <- get.source(source)

    df_brfss <- coi_data( coi = coi, subset = subset, year = year, geog= geog,
                          extent = extent, source = source,
                          exclude = exclude)

  }

  ##
  ## calc numbef of subsets
  ##

  if(is.null(subset)) {
    nsubs<-0
  } else {
    nsubs<-length(subset)
  }

  df_counts <- df_brfss %>%
    group_by_at(coi) %>% summarise(n=n(),wt = sum(FINAL_WT)) %>%
    mutate(pct = round(n/sum(n)*100,2)) %>%
    mutate(pct_wt = wt/sum(wt)*100)

  options(survey.lonely.psu = "adjust")

  # Create survey design
  brfssdsgn <- svydesign(
    id=~1,
    strata = ~STRATUM,
    weights = ~FINAL_WT,
    data = df_brfss)

  frmla1<- as.formula(paste0("~factor(",coi,")"))

  x <- svymean(frmla1,
               brfssdsgn,
               na.rm = TRUE) %>%
    as.data.frame() %>%
    mutate(Response = gsub("^factor.*?[)]","",row.names(.))) %>%
    mutate(mean = round(mean*100,2)) %>%
    mutate(SE = round(SE*100,3)) %>%
    mutate(CI_L = round(mean - 1.96*SE,2)) %>%
    mutate(CI_U = round(mean + 1.96*SE,2)) %>%
    rename(percent = mean) %>%
    select(Response, percent, starts_with("CI")) %>%
    remove_rownames()

  x %>% left_join(df_counts %>% select({{coi}},n), by=c("Response" = coi)) %>%
    relocate(n, .after = Response)

}

#' Simple Percents and Confidence Intervals
#'
#' Get the percent and CI for a column of data
#'
#' @param df_brfss data.frame: the data frame with the columns nedded
#' @param year integer: year of interest
#' @param geog character: geography of interest
#' @param coi character: column of interest
#' @param wt character: column name of weights
#' @param strata character: column name of strata
#' @param exclude character: pattern for excludes
#'
#' @return Object of class "svystat"
#' @export
#'
#' @examples
simple_stats_SAVE <- function(df_brfss = NULL, year = NULL, geog = NULL, extent = NULL, source = NULL,
                         coi, subset = NULL,
                         exclude = c("Don.*t|Refuse")) {

  require(dplyr)
  require(magrittr)
  require(survey)
  require(tibble)

  # if data frame not provided then get it from
  if(is.null(df_brfss)) {
    year <- get.year(year)
    geog <- get.geog(geog)
    extent <- get.extent(extent)
    source <- get.source(source)

    df_brfss <- coi_data( coi = coi, subset = subset, year = year, geog= geog,
                          extent = extent, source = source,
                          exclude = exclude)

  }

  ##
  ## calc numbef of subsets
  ##

  if(is.null(subset)) {
    nsubs<-0
  } else {
    nsubs<-length(subset)
  }

  df_counts <- df_brfss %>%
    group_by_at(coi) %>% summarise(n=n(),wt = sum(FINAL_WT)) %>%
    mutate(pct = round(n/sum(n)*100,2)) %>%
    mutate(pct_wt = wt/sum(wt)*100)

  options(survey.lonely.psu = "adjust")

  # Create survey design
  brfssdsgn <- svydesign(
    id=~1,
    strata = ~STRATUM,
    weights = ~FINAL_WT,
    data = df_brfss)

  frmla1<- as.formula(paste0("~factor(",coi,")"))

  x <- svymean(frmla1,
               brfssdsgn,
               na.rm = TRUE) %>%
    as.data.frame() %>%
    mutate(Response = gsub("^factor.*?[)]","",row.names(.))) %>%
    mutate(mean = round(mean*100,2)) %>%
    mutate(SE = round(SE*100,3)) %>%
    mutate(CI_L = round(mean - 1.96*SE,2)) %>%
    mutate(CI_U = round(mean + 1.96*SE,2)) %>%
    rename(percent = mean) %>%
    select(Response, percent, starts_with("CI")) %>%
    remove_rownames()

  x %>% left_join(df_counts %>% select({{coi}},n), by=c("Response" = coi)) %>%
    relocate(n, .after = Response)

}

coi_data_vers<- function(coi=NULL, subset = NULL, year = NULL, geog = NULL, extent = NULL,
                         source = NULL, version = 0) {

  year <- get.year(year)
  geog <- get.geog(geog)
  extent <- get.extent(extent)
  source <- get.source(source)

  vwt <- apply.pattern("weight_col",VERS=version)
  stratum <- apply.pattern("stratum_col")

  df <- brfss_data(year = year, geog= geog, extent = extent, source = source,  version = version)

  if(is.null(df)) return(data.frame())

  df%>%
    rename(FINAL_WT = {{vwt}}) %>%
    rename(STRATUM = {{stratum}}) %>%
    select({{coi}}, FINAL_WT, STRATUM, all_of(subset)) %>%
    mutate(vers = {{version}}) %>%
    na.exclude()

  # df%>%
  #   select({{coi}}, `_LLCPWT`, `_STSTR`) %>%
  #   mutate(vers = {{version}}) %>%
  #   na.exclude()
  #filter(!is.na(coi))

}

coi_data <- function( coi = NULL, year = NULL, geog = NULL,  extent =  NULL ,
                      source = NULL, subset = NULL, exclude = NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)
  source <- get.source(source)
  extent <- get.extent(extent)

  #df_brfss <- brfss_data(year,geog)
  df_brfss <- data.frame() #   coi_data(coi, year,geog,version = 0)

  invisible(
    sapply(0:highest_version(year),function(ver) {
      df_brfss <<- df_brfss %>% bind_rows(coi_data_vers(coi = coi, subset = subset, year = year,
                                                        geog = geog, extent = extent,
                                                        source = source,
                                                        version = ver))
    })
  )

  voi <- df_brfss %>% pull(vers) %>% unique()

  df_resp <- responses_by_geog(year,geog) %>%
    filter(version %in% voi) %>%
    mutate(pct = responses/sum(responses))

  df_brfss <- df_brfss %>%
    left_join(df_resp, by = c("vers" = "version")) %>%
    mutate(FINAL_WT = FINAL_WT * pct) %>%
    select(coi,FINAL_WT, STRATUM, all_of(subset))%>%
    rename(coi = {{coi}})%>%
    mutate(coi = replace(coi, grep(exclude,coi),NA)) %>%
    na.exclude() %>%
    mutate(coi = droplevels(coi)) %>%
    rename({{coi}} := coi)


  df_brfss
}
