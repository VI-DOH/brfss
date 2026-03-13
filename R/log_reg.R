
#' #'
#' #' Survey Statistics - Logistical Regression
#' #'
#' #' Retrieve survey statistics from survey data set for a specific column with optional subsetting by other column(s) and
#' #' optional weighting. This coerces the values in num_vals (numerator values) to 'Yes' and all values in den_vals (denominator values)
#' #' not in num_vals to 'No'
#' #'
#' #' Retuns a data.frame containing columns identiying the calculated numerators, denominators, means, standard error, and
#' #' confidence interval (lower and upper) for either the entire column of interest, or subsets of that column
#' #' #'
#' #'
#' #' @param df_data - data.frame: the survey data
#' #' @param depvar - character: dependent variable
#' #' @param exclude - integer: values in coi to exclude for analysis purposes
#' #' @param indepvar - character: name of column to subset data by
#' #' @param conf - numeric: confidence level, default is .95
#' #' @param weighted - logical: use weighting
#' ##'
#' #' @return data.frame: statistics for the coi each subset
#' #' @export
#' #'
#' #' @examples
#' #'
#' log_reg<-function(df_data = NULL, depvar, exclude = c("Don.*t|Refuse"), indepvar = NULL,
#'                   subset_by = NULL, sub_exclude = c("Don.*t|Refuse"),
#'                   conf=.95, weighted = TRUE, pct = FALSE, digits = 99) {
#'
#'   require(survey, quietly = T, warn.conflicts = F)
#'   require(dplyr, quietly = T, warn.conflicts = F)
#'
#'   df_brfss <- coi_data(df_data = df_data, coi = coi,   subvars = subsets, exclude = exclude)
#'
#'   test <- df_brfss %>% pull(.env$coi)
#'
#'   if(!(is.factor(test) && length(levels(test))>1) ) return(NULL)
#'
#'   # get data from
#'
#'   if(nrow(df_brfss)==0) {
#'     ret<-data.frame()
#'     return (ret)
#'   }
#'
#'
#'   # only the valid values - remove the excludes
#'
#'   ##########################
#'   ##
#'   ##  survey package
#'   ##
#'   ##
#'   if(weighted) weighting<-reformulate("FINAL_WT")   else weighting=NULL
#'   strata<-reformulate("STRATUM")   #else weights=NULL
#'
#'   #  ids<- reformulate(all_vals)
#'
#'   options(survey.lonely.psu = "adjust")
#'
#'   frmla<- as.formula(c(indepvar) %>% paste0("`",.,"` ~") %>% paste0(.,paste0("`",depvar,"`")))
#'   frmla <- c("SEXVAR","HLTHPLN1")  %>% paste0("`",.,"`") %>% reformulate()
#'
#'   des<-survey::svydesign(ids = ~1,
#'                          strata = strata,
#'                          variables =  frmla,
#'                          data = df_brfss,
#'                          weights = weighting,
#'                          deff=F)
#'
#'   df_stats_main <- stats_no_subs(des, pct = pct, digits = digits)
#'
#'   df_subs <- data.frame()
#'
#'   if (nsubs > 0) {
#'     invisible(
#'       sapply(subsets, function(subset) {
#'         cols <- c(coi,subset)  %>% paste0("`",.,"`")
#'         frmla<- reformulate(cols)
#'
#'         des<-survey::svydesign(ids = ~1,
#'                                strata = strata,
#'                                variables =  frmla,
#'                                data = df_brfss,
#'                                weights = weighting,
#'                                deff=F)
#'
#'         df_subs <<- df_subs %>%
#'           bind_rows(stats_w_subs(des, pct = pct, digits = digits))
#'
#'       })
#'     )
#'
#'     df_subs <- df_subs %>% filter(!grepl(sub_exclude,subset))
#'   }
#'
#'   df <- bind_rows(df_stats_main, df_subs )
#'
#'   df_lo <- get.layout() %>%
#'     filter(col_name == .env$coi)
#'
#'   attr(df,"question") <- df_lo %>%
#'     pull(question)
#'
#'   attr(df,"label") <- unname(df_lo %>%
#'                                pull(label))
#'
#'   attr(df,"coi") <- coi
#'
#'   attr(df,"weighted") <- weighted
#'
#'   df
#' }

