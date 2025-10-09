
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
survey_stats <- function(df_data = NULL, coi, exclude = c("Don.*t|Refuse"), subsets = NULL,
                         subset_by = NULL, sub_exclude = c("Don.*t|Refuse"),
                         conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {

  require(survey, quietly = T, warn.conflicts = F)
  require(dplyr, quietly = T, warn.conflicts = F)


  ## make sure that this column exists

  if(!has_column(df_data, coi)) return(NULL)

  ##    this is a kludgy way to remove the exclude ...
  ##      NULL would be better and that will be fixed

  if(is.null(exclude)) exclude <- "^$"

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


  ## this will fail if there is only one level so
  ##    we must add a"dummy" response level
  ##      it will be removed before returning the data

  levels <- levels(df_brfss %>% pull({{coi}}))

  if(length(levels) == 1) {
    levels(df_brfss[,coi]) <- c(levels , 'dummy')
  }

  des <- NULL

  des <- tryCatch({
    survey::svydesign(ids = ~1,
                      strata = strata,
                      variables =  frmla,
                      data = df_brfss,
                      weights = weighting,
                      deff=F)
  }, error = function(e) {

    cat(" I caught an error \n  ----- ", e$message, "\n ------\n")
    return(NULL)

  }, warning = function(w) {

    cat(" warning", w$message, "\n")
    return(NULL)

    # }, finally = function() {
    #   cat(" finally \n")
    #
  }  )

  if(is.null(des)) {
    return(data.frame())

  }

  df_stats_main <- stats_no_subs(des, pct = pct, digits = digits)

  df_subs <- data.frame()

  if (nsubs > 0) {

    df_subs <- map(subsets, function(subset){
      cols <- c(coi,subset)  %>% paste0("`",.,"`")
          frmla<- reformulate(cols)

          des<-survey::svydesign(ids = ~1,
                                 strata = strata,
                                 variables =  frmla,
                                 data = df_brfss,
                                 weights = weighting,
                                 deff=F)


          stats_w_subs(des, pct = pct, digits = digits)

        }) %>% bind_rows()

  }

  df <- bind_rows(df_stats_main, df_subs )

  df_lo <- get.layout()

  if(is.null(df_lo)) {
    df_lo <- layout_from_data(df_data)
  }

  df_lo <- df_lo %>%
    filter(col_name == {{coi}})

  population <- pop_sex(df_data, coi) %>% gsub("ale","ales",.)

  label <- unname(df_lo %>% pull(label))
  if(length(label) == 0) {
    label <- attr(df_data[[coi]], "label")
  }



  # remove "dummy" response (in case there was only one level)

  df <- df %>% filter(response != "dummy")

  ## return data.frame

  structure(df,
            class = c("brfss_stats", "data.frame"),
            coi = coi,
            population =population,
            section = df_lo %>% pull(section),
            question = df_lo %>% pull(question),
            label = label,
            weighted = weighted,
            conf = conf)
}

###########################################################
##
##    get stats for a coi with subsetting

stats_w_subs <- function(des, conf = .95, pct = TRUE, digits = 2) {

  mult <- ifelse(pct,100,1)

  coi <- names(des$variables[1])
  subset <- names(des$variables[2])
  subvar <- names(des$variables[2])

  frmla1<- reformulate(c(coi) %>% paste0("`",.,"`"))
  frmla2<- reformulate(c(subset) %>% paste0("`",.,"`"))

  mysvymean<-survey::svyby(frmla1,frmla2,des,svymean)

  mysvytotal <-survey::svyby(frmla1,frmla2,des,svytotal) %>%
    select(-starts_with("se."))  %>%
    rename_with(~gsub(coi,"",.x))  %>%
    rename_with(~gsub("`","",.x))  %>%
    tidyr::pivot_longer(
      cols = -all_of(subvar),
      names_to = "response",
      values_to = "num_wtd"
    )  %>%
    rename(subset = 1)

  df_wtd <- mysvytotal  %>%
    group_by(subset) %>%
    summarize(den_wtd = sum(num_wtd)) %>%
    as.data.frame()

  mysvytotal <- mysvytotal %>%
    left_join(df_wtd, by = join_by(subset))

  mysvycounts<-survey::svyby(frmla1,frmla2,des,unwtd.count)

  # mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F) %>%
  #   as.data.frame() %>%
  #   mutate(response = rownames(.)) %>%
  #   mutate(response = gsub(coi, "", response))  %>%
  #   mutate(response = gsub("\`","",response))%>%
  #   select(-SE) %>%
  #   rename(num_wtd = total)  %>%
  #   mutate(den_wtd = sum(num_wtd)) %>%
  #   relocate(response)

  df_dens <- mysvycounts %>% select(-se) %>%
    rename( den = counts) %>%
    rename(subset = {{subset}})

  df_nums <- data.frame(des$variables[1],des$variables[2],check.names = FALSE) %>%
    group_by_at(c(coi, subset)) %>%
    summarise(num=n()) %>%
    rename(response = {{coi}}, subset = {{subset}})

  myci <- svyci(mysvymean, conf = conf) %>%
    remove_coi(coi) %>%
    mutate(subset = gsub("(.*)[:](.*)","\\1", response)) %>%
    mutate(response = gsub("(.*)[:](.*)","\\2", response)) %>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits))

  df_stats <- as.data.frame(mysvymean) %>%
    rename_with(~gsub(coi,"",.x)) %>%
    rename_with(~gsub("`","",.x))  %>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits))%>%
    tidyr::pivot_longer(
      cols = -all_of(subvar),
      names_to = "response",
      values_to = "value"
    ) %>%
    mutate(
      type = if_else(stringr::str_starts(response, "se\\."), "se", "percent"),
      response = stringr::str_remove(response, "^se\\.")
    ) %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = value
    ) %>%
    rename(subset = 1) %>%
    mutate(subvar = {{subvar}}) %>%
    relocate(subvar) %>%
    left_join(myci, by = join_by(response, subset)) %>%
    mutate(cv = se/percent) %>%
    left_join(df_dens, by = join_by(subset))%>%
    left_join(df_nums, by = join_by(subset, response)) %>%
    left_join(mysvytotal, by = join_by(subset, response)) %>%
    as.data.frame() %>%
    relocate(c(num,den), .after = response)

  #df_stats

  df_stats
}

###########################################################
##
##    get stats for just a coi


stats_no_subs <- function(des, conf = .95, pct = TRUE, digits = 2) {

  frmla<- reformulate(names(des$variables) %>% paste0("`",.,"`"))
  mult <- ifelse(pct,100,1)
  #
  coi <- as.character(frmla)[2]

  mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)

  mycv <- cv(mysvymean)%>% t()
  mycv <- data.frame(response = dimnames(mycv)[[2]] %>% gsub(coi,"",.), cv = mycv %>% as.numeric())

  myci <- svyci(mysvymean = mysvymean, conf = conf) %>%
    remove_coi(coi)%>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits))

  mysvymean <- mysvymean %>%
    as.data.frame() %>%
    mutate(response = rownames(.))  %>%
    remove_coi(coi) %>%
    rename(se = SE) %>%
    rename(percent = mean) %>%
    relocate(response) %>%
    tibble::remove_rownames()%>%
    mutate(across(where(is.numeric), ~ . * mult)) %>%
    mutate(across(where(is.numeric), round, digits))

  mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F) %>%
    as.data.frame() %>%
    mutate(response = rownames(.))  %>%
    remove_coi(coi) %>%
    select(-SE) %>%
    rename(num_wtd = total)  %>%
    mutate(den_wtd = sum(num_wtd)) %>%
    relocate(response) %>%
    tibble::remove_rownames()

  mysvycounts <- survey::svyby(formula = frmla, by = frmla, design = des, FUN = unwtd.count)%>%
    as.data.frame() %>%
    select(-se) %>%
    rename(response = 1)  %>%
    rename(num = counts) %>%
    mutate(den = sum(num)) %>%
    relocate(response) %>%
    tibble::remove_rownames()

  names(mysvymean) <- gsub(coi,"",names(mysvymean))

  df_stats <- mysvycounts  %>%
    left_join(mysvytotal, by = join_by(response)) %>%
    left_join(mysvymean, by = join_by(response)) %>%
    left_join(mycv, by = join_by(response)) %>%
    left_join(myci, by = join_by(response))%>%
    mutate(subvar = "") %>%
    mutate(subset = "All Respondents")  %>%
    relocate(c(subvar,subset))

  rownames(df_stats)<- NULL

  df_stats
}

# add_CV <- function(df, mysvymean,coi) {
#
#   # has_subs <- !all(is.na(df$subset))
#
#   if(has_subs) {
#     df_cv <- as.data.frame(cv(mysvymean))%>%
#       t() %>%
#       as.data.frame() %>%
#       mutate(response = rownames(.)) %>%
#       mutate(response = gsub(paste0("^se.*",coi),"",response)) %>%
#       mutate(response = gsub("[`]","",response)) %>%
#       tibble::remove_rownames() %>%
#       reshape2::melt(value.name = "cv", id.vars = "response") %>%
#       rename(subset = variable)
#
#   } else {
#     df_cv <- as.data.frame(cv(mysvymean))%>%
#       rename(cv = 1) %>%
#       mutate(response = rownames(.)) %>%
#       tibble::remove_rownames() %>%
#       mutate(response = gsub(coi,"",response))
#   }
#
#
#   join_by = "response"
#
#   if(has_subs) join_by = c(join_by,"subset")
#
#
#   df <- df %>%
#     left_join(df_cv, by = join_by)
#
#   df
# }

remove_coi <- function(df, coi) {

  df %>%
    mutate(response = gsub(coi,"",response)) %>%
    mutate(response = gsub("\`","",response))
}

svyci <- function(mysvymean, conf) {

  as.data.frame(confint(mysvymean,level = conf))%>%
    rename(CI_lower = 1, CI_upper = 2) %>%
    mutate(response = rownames(.)) %>%
    tibble::remove_rownames()

}


add_CI <- function(df, mysvymean, coi, conf) {

  df_ci <- as.data.frame(confint(mysvymean,level = conf))%>%
    rename(CI_lower = 1, CI_upper = 2) %>%
    mutate(response = rownames(.)) %>%
    tibble::remove_rownames() %>%
    mutate(response = gsub(coi,"",response)) %>%
    mutate(response = gsub("\`","",response))

  resps <- df_ci %>% pull(response)
  has_subs <- length(grep(":",resps)) == length(resps)

  df_ci <- df_ci %>%
    mutate(subset = ifelse(grepl(":",response),
                           gsub("(.*):(.*)","\\1",response),NA)) %>%

    mutate(response = ifelse(grepl(":",response),
                             gsub("(.*):(.*)","\\2",response),response))

  join_by = "response"

  if(has_subs) join_by = c(join_by,"subset")

  df <- df %>%
    left_join(df_ci, by = join_by) %>%
    mutate(CI_lower = ifelse(CI_lower <0 ,0,CI_lower)) %>%
    mutate(CI_upper = ifelse(CI_upper>1,1,CI_upper))

  df
}


add_CI_SAVE <- function(df, conf) {

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

  x %>% left_join(df_counts %>% select(all_of(coi),n), by=c("Response" = coi)) %>%
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

#' Default Exclude Expression
#'
#' @return char exclude
#' @export
#'
#' @examples
exclude_default <- function() {c("Don.*t|Refuse")}
