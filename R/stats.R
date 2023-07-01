
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
#' @param df_data - data.frame: the survey data
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
survey_stats<-function(df_data = NULL, coi, exclude = c("Don.*t|Refuse"), subsets = NULL,
                       subset_by = NULL, sub_exclude = c("Don.*t|Refuse"),
                       conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {

  require(survey, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)

  ##    this is a kludgy way to remove the exclude ... NULL would be better and that will be fixed

  if(is.null(exclude)) exclude <- "DHFHREYDHDMJDNCNJ"

  ###########################################################

  df_brfss <- coi_data(df_data = df_data, coi = coi, subsets = subsets, exclude = exclude)

  test <- df_brfss %>% pull({{coi}})

  if(!(is.factor(test) && length(levels(test))>1) ) {
    #return(NULL)
    df_brfss<- df_brfss %>% mutate({{coi}} := as.factor(.[[coi]]))

  }
  # get data from

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

        df_subs <<- df_subs %>%
          bind_rows(stats_w_subs(des, pct = pct, digits = digits))

      })
    )

    df_subs <- df_subs %>% filter(!grepl(sub_exclude,subset))
  }

  df <- bind_rows(df_stats_main, df_subs )

  df_lo <- get.layout() %>%
    filter(col_name == {{coi}})

  attr(df,"question") <- df_lo %>%
    pull(question)

  attr(df,"label") <- unname(df_lo %>%
                               pull(label))

  attr(df,"coi") <- coi

  attr(df,"weighted") <- weighted

  df
}

###########################################################
##
##    get stats for a coi with subsetting

stats_w_subs <- function(des, conf = .95, pct = TRUE, digits = 2) {

  mult <- ifelse(pct,100,1)

  coi <- names(des$variables[1])
  subset <- names(des$variables[2])

  frmla1<- reformulate(c(coi) %>% paste0("`",.,"`"))
  frmla2<- reformulate(c(subset) %>% paste0("`",.,"`"))

  mysvymean<-survey::svyby(frmla1,frmla2,des,svymean)
  mysvytotal<-survey::svyby(frmla1,frmla2,des,svytotal)
  mysvycounts<-survey::svyby(frmla1,frmla2,des,unwtd.count)

  df_dens <- mysvycounts %>% select(-se) %>%
    rename( den = counts) %>%
    rename(subset = {{subset}})

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
    mutate(across(where(is.numeric), round, digits))


  df_stats <- df_stats %>%
    left_join(df_nums, by = c("subset", "response")) %>%
    replace(is.na(.), 0) %>%
    select(subvar, subset, response, num, percent, se, starts_with("CI"))



  df_stats %>% left_join(df_dens, by = c("subset"))%>%
    select(subvar, subset, response, den, num, percent, se, starts_with("CI"))
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
    mutate(den = sum(num)) %>%
    relocate(response, .before = 1) %>%
    relocate(den, .after = 1) %>%
    mutate(subvar = "") %>%
    mutate(subset = "All Respondents") %>%
    select(subvar, subset, response, den, num, percent, se, starts_with("CI"))

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
simple_stats <- function(df_brfss = NULL, year = NULL,
                         geog = NULL, extent = NULL, source = NULL,
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
  ## calc number of subsets
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

RSE <- function(pct,se) {
  se/pct*100
}

#' Add Relative Standard Error
#'
#' Add the relative standard error (RSE) to a data frame that contains the standard error and percent
#'
#' @param df  data frame with columns containing both standard error and percent
#'
#' @return
#' @export
#'
#' @examples
add_RSE <- function(df, se_col = "se", pct_col = "percent") {


  df %>% mutate(RSE = !!sym(se_col) / !!sym(pct_col) * 100)
}


# BRFSS_SignificanceFunc()
# Creates subsets of descriptive statistics data set
# Performs pairwise comparisons of 95% CI
# Produces modifiable .doc file
# Non-overlapping 95% CI (prevalence and adult values)
# Suppressed data (default: RSE > 30, n < 50)
# Data visualizations (optional)
#
# Health Outcome: Respondent did not have a personal health care provider
# Idaho BRFSS 2020 (proc descript: 393 rows)
# Demographic/crossing variables
# District, Physical/Mental Health, Sex, Age, Sex by Age, Depression Diagnosis, Education, Employment, Ethnicity, Income, Marital Status, Nicotine Use, Veteran Status

sig_diffs <- function(df) {

  #xing_vars <- Physical/Mental Health, Sex, Age, Sex by Age, Depression Diagnosis, Education, Employment, Ethnicity, Income, Marital Status, Nicotine Use, Veteran Status

  xing_vars <- c("SEX","EDUCATION", "MARITAL","VETERAN","AGE")


  fctrs <- df %>% Filter(is.factor,.) %>% names()



}

#'
#' Survey Statistics - Logistical Regression
#'
#' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' optional weighting. This coerces the values in num_vals (numerator values) to 'Yes' and all values in den_vals (denominator values)
#' not in num_vals to 'No'
#'
#' Retuns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#' #'
#'
#' @param df_data - data.frame: the survey data
#' @param depvar - character: dependent variable
#' @param exclude - integer: values in coi to exclude for analysis purposes
#' @param indepvar - character: name of column to subset data by
#' @param conf - numeric: confidence level, default is .95
#' @param weighted - logical: use weighting
##'
#' @return data.frame: statistics for the coi each subset
#' @export
#'
#' @examples
#'
log_reg<-function(df_data = NULL, depvar, exclude = c("Don.*t|Refuse"), indepvar = NULL,
                  subset_by = NULL, sub_exclude = c("Don.*t|Refuse"),
                  conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {

  require(survey, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)

  df_brfss <- coi_data(df_data = df_data, coi = coi, subsets = subsets, exclude = exclude)

  test <- df_brfss %>% pull({{coi}})

  if(!(is.factor(test) && length(levels(test))>1) ) return(NULL)

  # get data from

  if(nrow(df_brfss)==0) {
    ret<-data.frame()
    return (ret)
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

  frmla<- as.formula(c(indepvar) %>% paste0("`",.,"` ~") %>% paste0(.,paste0("`",depvar,"`")))
  frmla <- c("SEXVAR","HLTHPLN1")  %>% paste0("`",.,"`") %>% reformulate()

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

        df_subs <<- df_subs %>%
          bind_rows(stats_w_subs(des, pct = pct, digits = digits))

      })
    )

    df_subs <- df_subs %>% filter(!grepl(sub_exclude,subset))
  }

  df <- bind_rows(df_stats_main, df_subs )

  df_lo <- get.layout() %>%
    filter(col_name == {{coi}})

  attr(df,"question") <- df_lo %>%
    pull(question)

  attr(df,"label") <- unname(df_lo %>%
                               pull(label))

  attr(df,"coi") <- coi

  attr(df,"weighted") <- weighted

  df
}


