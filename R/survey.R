
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
brfss_data <- function() {

  params <- my.brfss.patterns()

  fname <- brfss_data_path(rw = 'r')

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
survey_stats<-function(coi, exclude = c("Don.*t|Refuse"), subsets = NULL, subset_by = NULL,
                       conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {

  require(survey, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)

  if(!brfss_data() %>% pull({{coi}}) %>% is.factor()) return(NULL)

  # get data from
  df_brfss <- coi_data( coi = coi, subsets = subsets, exclude = exclude)

  if(nrow(df_brfss)==0) {
    ret<-data.frame()
    return (ret)
  }

  if(is.null(subsets)) {
    nsubs<-0
  } else {
    nsubs<-length(subsets)
  }

  # only the valid values - remove the excludes

  ##########################
  ##
  ##  survey package
  ##
  ##
  if(weighted) weighting<-reformulate("FINAL_WT")   else weighting=NULL
  strata<-reformulate("STRATUM")   #else weights=NULL

  #  ids<- reformulate(all_vals)

  options(survey.lonely.psu = "adjust")

  frmla<- reformulate(c(coi) %>% paste0("`",.,"`"))

  des<-survey::svydesign(ids = ~1,
                         strata = strata,
                         variables =  frmla,
                         data = df_brfss,
                         weights = weighting,
                         deff=F)

  df_stats_main <- stats_no_subs(des, pct = pct, digits = digits)

  df_subs <- data.frame()

  if (nsubs > 0) {


    invisible(
      sapply(subsets, function(subset) {
        cols <- c(coi,subset)  %>% paste0("`",.,"`")
        frmla<- reformulate(cols)

        des<-survey::svydesign(ids = ~1,
                               strata = strata,
                               variables =  frmla,
                               data = df_brfss,
                               weights = weighting,
                               deff=F)

        df_subs <<- df_subs %>% bind_rows(stats_w_subs(des, pct = pct, digits = digits))

      })
    )

  }

  df <- bind_rows(df_stats_main, df_subs)

  attr(df$response,"coi") <- coi


  df
}

###########################################################
##
##    get stats for a coi with subsetting

stats_w_subs <- function(des, conf = .95, pct = TRUE, digits = 2) {

  mult <- ifelse(pct,100,1)

  coi <- names(des$variables[1])
  subset <- names(des$variables[2])

  nsubs<-length(subsets)

  frmla1<- reformulate(c(coi) %>% paste0("`",.,"`"))
  frmla2<- reformulate(c(subset) %>% paste0("`",.,"`"))

  mysvymean<-survey::svyby(frmla1,frmla2,des,svymean)
  mysvytotal<-survey::svyby(frmla1,frmla2,des,svytotal)
  mysvycounts<-survey::svyby(frmla1,frmla2,des,unwtd.count)

  df_nums <- data.frame(des$variables[1],des$variables[2],check.names = FALSE) %>%
    group_by_at(c(coi, subset)) %>%
    summarise(num=n()) %>%
    rename(response = {{coi}}, subset = {{subset}})


  df_stats <- reshape::melt(as.data.frame(mysvymean), id.vars = subset)

  df_stats <- df_stats %>%
    filter(!grepl("^se[.]",variable)) %>%
    rename(percent = value) %>%
    left_join(
      df_stats %>%
        filter(grepl("^se[.]",variable)) %>%
        mutate(variable = gsub("^se[.]","",variable)) %>%
        rename(se = value),
      by = c("variable", subset)
    ) %>%
    mutate(variable = gsub(paste0("^",coi),"",variable)) %>%
    mutate(variable = gsub(paste0("^`",coi,"`"),"",variable)) %>%
    rename(subset = {{subset}}) %>%
    rename(response = variable) %>%
    mutate(subvar = names(des$variables[2])) %>%
    add_CI(conf) %>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits)) %>%
    left_join(df_nums, by = c("subset", "response")) %>%
    replace(is.na(.), 0) %>%
    select(subvar, subset, response, num, percent, se, starts_with("CI"))


  df_stats
}

###########################################################
##
##    get stats for just a coi


stats_no_subs <- function(des, conf = .95, pct = TRUE, digits = 2) {

  frmla<- reformulate(names(des$variables) %>% paste0("`",.,"`"))
  mult <- ifelse(pct,100,1)

  mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)
  mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F)
  mysvycounts<-survey::svyby(formula = frmla, by = frmla, design = des, FUN = unwtd.count)

  coi <- as.character(frmla)[2]
  names(mysvymean) <- gsub(coi,"",names(mysvymean))

  df_stats <- as.data.frame(mysvymean) %>%
    rename(se = SE) %>%
    rename(percent = mean) %>%
    add_CI(conf) %>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits)) %>%
    mutate(num = as.integer(mysvycounts$counts)) %>%
    mutate(response = rownames(.)) %>%
    relocate(response, .before = 1) %>%
    mutate(subvar = "") %>%
    mutate(subset = "") %>%
    select(subvar, subset, response, num, percent, se, starts_with("CI"))

  rownames(df_stats)<- NULL


  df_stats
}

add_CI <- function(df, conf) {
  #########################################################
  ##
  ##    calculate confidence interval

  sd<-qnorm(1-(1-conf)/2)

  df <- df %>%
    mutate(CI_lower = percent - se * sd) %>%
    mutate(CI_upper = percent + se * sd) %>%
    mutate(CI_lower = ifelse(CI_lower <0 ,0,CI_lower)) %>%
    mutate(CI_upper = ifelse(CI_upper>1,1,CI_upper))


  df
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


coi_data_vers<- function(coi=NULL, subsets = NULL, version = NULL) {

  vwt <- apply.pattern("weight_col",VERS=version)
  stratum <- apply.pattern("stratum_col")

  brfss.params(version = version)
  df <- brfss_data()

  if(is.null(df)) return(data.frame())

  df%>%
    rename(FINAL_WT = {{vwt}}) %>%
    rename(STRATUM = {{stratum}}) %>%
    select(all_of(coi), FINAL_WT, STRATUM, all_of(subsets)) %>%
    mutate(vers = {{version}}) %>%
    na.exclude()

  # df%>%
  #   select({{coi}}, `_LLCPWT`, `_STSTR`) %>%
  #   mutate(vers = {{version}}) %>%
  #   na.exclude()
  #filter(!is.na(coi))

}

coi_data <- function( coi = NULL, subsets = NULL, exclude = NULL) {


  #df_brfss <- brfss_data(year,geog)
  df_brfss <- data.frame() #   coi_data(coi, year,geog,version = 0)

  invisible(
    sapply(0:highest_version(),function(ver) {
      df_brfss <<- df_brfss %>% bind_rows(coi_data_vers(coi = coi, subsets = subsets,
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
    select(coi,FINAL_WT, STRATUM, all_of(subsets))%>%
    rename(coi = {{coi}})%>%
    mutate(coi = replace(coi, grep(exclude,coi),NA)) %>%
    na.exclude() %>%
    mutate(coi = droplevels(coi)) %>%
    rename({{coi}} := coi)


  df_brfss
}
#
# stats_no_subs <- function(frmla,des) {
#
#   mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)
#   mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F)
#
#   # coi <- as.character(frmla)[2]
#   # names(mysvymean) <- gsub(coi,"",names(mysvymean))
#
#   browser()
#
#   df_mean <- t(as.data.frame(mysvymean))
#   df_se <- t(as.data.frame(df_mean[2,]))
#   #df_cnts <- t(as.data.frame(mysvycounts$counts))
#
#   colnames(df_se)<-paste("se.fcoi",1:ncol(df_se),sep="")
#   #colnames(df_cnts)<-paste("num.fcoi",1:ncol(df_cnts),sep="")
#
#   df_stats<-cbind(t(as.data.frame(df_mean[1,])),df_se) # ,df_cnts)
#   rownames(df_stats)<- NULL
#
#
#   df_stats
# }

#' Stretch Stats Table
#'
#' Stretch (cast) the stats table horizontally such that measure values become the
#' column names. They are actually spanner values with the stat name (pct, etc.) as the
#' column label
#'
#' @param df_stats - data.frame
#'
#' @return
#' @export
#'
#' @examples
stats_wide <- function(df_stats) {
  require(gt)

  coi <-attr(df_stats$response,"coi")

  df_stats <- df_stats %>%
    mutate(CI = paste0(CI_lower, " - ", CI_upper))

  df_stats_wide <- df_stats %>%

    # stretch measure horizontally with variable mean
    reshape2::dcast(subvar + subset ~ response, value.var = c("num")) %>%

    # add CI_lower to right end
    left_join(df_stats %>%
                reshape2::dcast(subvar + subset ~ response, value.var = c("percent")),
              by = c("subset", "subvar"))  %>%

    rename_with(~ gsub(".x", ".num",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", "",.x, fixed = TRUE))%>%

    left_join(df_stats %>%
                reshape2::dcast(subvar + subset ~ response, value.var = c("CI")),
              by = c("subset", "subvar"))  %>%

    rename_with(~ gsub(".x", ".percent",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", ".CI",.x, fixed = TRUE))

  fctrs <- levels(brfss_data() %>% pull({{coi}}) %>% droplevels() )
  stats <- c("num", "percent", "CI") #,"CI_lower","CI_upper")

  df_col_order <- data.frame(name = colnames(df_stats_wide)) %>%
    mutate(is_meas = grepl(".",name, fixed=TRUE)) %>%
    mutate(var = gsub("(.*)\\.(.*)","\\1",name))%>%
    mutate(stat = gsub("(.*)\\.(.*)","\\2",name)) %>%
    mutate(varpos = ifelse(is_meas,match(var,fctrs),0)) %>%
    mutate(statpos = ifelse(is_meas,match(stat,stats),0)) %>%
    arrange(varpos,statpos)

  col_order <- df_col_order %>%
    pull(name)

  n_non <- df_col_order %>% filter(varpos == 0) %>% nrow

  df_spanners <- df_col_order %>%
    filter(is_meas) %>%
    group_by(var,varpos) %>%
    summarize(tot = max(statpos)) %>%
    arrange(varpos)


  df_stats_wide <- df_stats_wide  %>% select(all_of(col_order))


  lbls <- as.list(mapply(function(nm,val) {
    x <- val
    x
  }, df_col_order$name, df_col_order$stat))

  lbls[lbls=="percent"] <- "%"


  gt_stats <- gt::gt(df_stats_wide,
                     groupname_col= "subvar") %>%
    cols_label( .list =lbls)

  sapply(1:max(df_col_order$varpos), function(i) {

    gt_stats <<- gt_stats %>%

      tab_spanner(label = df_spanners[i,"var"],
                  columns = df_col_order %>% filter(varpos == i) %>% pull(name) )

  })

  gt_stats
}


