
library(R6)
#' Layout_Mgr R6 Class
#'
#' @export
StatsMgr <-
  R6Class(classname = "StatsMgr",


          ################################################################################
          ##
          ##           PRIVATE
          ##
          ################################################################################

          private = list(
            data_pvt = NULL,
            data_mgr_pvt = NULL,
            coi_pvt = NULL,
            stats_pvt = c("subvar","subset","response","num","den",
                          "percent","se","CI_lower","CI_upper","cv",
                          "percent_unwtd","num_wtd","den_wtd"),
            subsets_pvt = NULL,
            subpopulation_pvt = NULL,
            exclude_pvt = c("Don?t|Refuse"),
            sub_exclude_pvt = c("Don?t|Refuse"),
            conf_pvt =.95,
            weighted_pvt = TRUE,
            weight_col_pvt = "_LLCPWT",
            pct_pvt = FALSE,
            digits_pvt = 99,
            my_stats_pvt = NULL,
            reduce_pvt = FALSE

          ),

          ################################################################################
          ##
          ##           PUBLIC
          ##
          ################################################################################

          public = list(
            initialize = function(data = NULL, data_mgr = NULL, coi = "",
                                  exclude = c("Don.*t|Refuse"),
                                  subsets = NULL,
                                  sub_exclude = c("Don.*t|Refuse"),
                                  subpopulation = NULL,
                                  conf =.95,
                                  weighted = TRUE,
                                  weight_col = "_LLCPWT",
                                  pct = FALSE,
                                  digits = 99,
                                  reduce = FALSE) {

              private$data_pvt <- data
              private$data_mgr_pvt <- data_mgr
              private$coi_pvt <- coi
              private$exclude_pvt <- exclude
              private$subsets_pvt <-subsets
              private$subpopulation_pvt <- subpopulation
              private$sub_exclude_pvt <-sub_exclude
              private$conf_pvt <-conf
              private$weighted_pvt <- weighted
              private$weight_col_pvt <- weight_col
              private$pct_pvt <- pct
              private$digits_pvt <- digits
              private$reduce_pvt <- reduce

              private$my_stats_pvt <- private$stats_pvt

            },

            show_all_subs = function() {

              self$sub_exclude <- "^$"

            },

            show_all_responses = function() {

              self$exclude <- "^$"

            },

            remove_stats = function(pttrn) {

              private$my_stats_pvt <-
                private$my_stats_pvt %>%
                grep(pttrn, ., value = T, invert = T)
            },

            reset_stats = function() {

              private$my_stats_pvt <- private$stats_pvt
            },

            survey_stats = function(coi = NULL, weighted = NULL, reduce = NULL, wide = FALSE) {

              pvt <- private

              coi <- ifelse(is.null(coi), pvt$coi_pvt,coi)

              if(is.null(coi) || nchar(coi) == 0 || length(coi) == 0) {
                message("must select a valid column of interest (coi)")
                return(NULL)
              }

              weighted <- ifelse(is.null(weighted), pvt$weighted_pvt,weighted)
              reduce <- ifelse(is.null(reduce), pvt$reduce_pvt,reduce)

              if(!is.null(pvt$data_mgr_pvt)) pvt$data_pvt <- pvt$data_mgr_pvt$prepped_data

              df <- survey_stats(
                df_data = pvt$data_pvt,
                coi = coi,
                exclude = pvt$exclude_pvt,
                subsets = pvt$subsets_pvt,
                subset_by = pvt$subset_by_pvt,
                sub_exclude = pvt$sub_exclude_pvt,
                conf = pvt$conf_pvt,
                weighted = weighted,
                weight_col = pvt$weight_col_pvt,
                pct = pvt$pct_pvt,
                digits = pvt$digits_pvt)



              if(reduce) {

                df <- df %>%
                  select(-any_of(c("num", "den", "percent_unwtd"))) %>%
                  rename_with(.fn = ~gsub("_wtd","", .x))
              }

              rm <- setdiff(pvt$stats_pvt, pvt$my_stats_pvt)

              df <- df %>% filter(response != "dummy") %>%
                select(-all_of(rm))

              if(wide) df <- self$widen(df)

              return(df)


            },

            widen = function(df_stats = NULL, coi = NULL,
                             wide_stats = c("den", "num", "percent"), sep_char = "^") {


              if(is.null(df_stats)) {

                df_stats <- self$survey_stats(coi)

              }

              if("ci" %in% wide_stats && "CI_lower" %in% colnames(df_stats)) {
                df_stats <- df_stats %>%
                  mutate(ci = paste0(CI_lower, "-", CI_upper))
              }
              # get the set of valid values

              # vals <- values() %>% filter(col_name == coi) %>% pull(text)

              vals <- df_stats %>% pull(response) %>% unique()
              nvals <- length(vals)

              stats_in <- df_stats %>% select( where(is.numeric), matches("^ci$")) %>% colnames()
              stats_rm <- stats_in %>% {.[!. %in% wide_stats]}

              df_stats <- df_stats %>% select(-all_of(stats_rm))

              stats <- wide_stats %>% {.[!. %in% "den"]} %>% {.[.%in% stats_in]}

              cnames <- df_stats %>% colnames()
              nstats <- length(stats)

              static_cols <- cnames %>% {.[!. %in% c(stats, "response")]}

              nstatic <- length(static_cols)

              has_subvar <- "subvar" %in% static_cols

              fin_cols <- c(static_cols,
                            expand.grid(stats,vals) %>%
                              mutate(col = paste0(Var1,sep_char,Var2)) %>%
                              pull(col))

              if(length(stats) == 1) fin_cols <- gsub(".*\\^","", fin_cols)

              if(has_subvar) subvars <- df_stats %>%
                pull(subvar) %>%
                unique()
              else subvars  <-  character(0)

              df_stats %>%
                tidyr::pivot_wider(names_from = response,
                                   values_from = all_of(stats),
                                   names_sep = sep_char) %>%
                as.data.frame() %>%
                select(all_of(fin_cols))

            }




          ),

          ################################################################################
          ##
          ##           ACTIVE
          ##
          ################################################################################

          active = list(

            survey_data = function(value) {

              if(missing(value)) return(private$data_pvt)

              private$data_pvt <- value

            },

            coi = function(value) {

              if(missing(value)) return(private$coi_pvt)

              private$coi_pvt <- value

            },

            exclude = function(value) {

              if(missing(value)) return(private$exclude_pvt)

              private$exclude_pvt <- value

            },

            subsets = function(value) {

              if(missing(value)) return(private$subsets_pvt)

              private$subsets_pvt <- value

            },

            subpopulation = function(value) {

              if(missing(value)) return(private$subpopulation_pvt)

              private$subpopulation_pvt <- value

            },

            sub_exclude =function(value) {

              if(missing(value)) return(private$sub_exclude_pvt)

              private$sub_exclude_pvt <- value

            },

            conf =function(value) {

              if(missing(value)) return(private$conf_pvt)

              private$conf_pvt <- value

            },

            weighted = function(value) {

              if(missing(value)) return(private$weighted_pvt)

              private$weighted_pvt <- value

            },

            weight_col = function(value) {

              if(missing(value)) return(private$weight_col_pvt)

              private$weight_col_pvt <- value

            },

            pct = function(value) {

              if(missing(value)) return(private$pct_pvt)

              private$pct_pvt <- value

            },

            digits = function(value) {

              if(missing(value)) return(private$digits_pvt)

              private$digits_pvt <- value

            },

            stats = function(value) {

              if(missing(value)) return(private$stats_pvt)

              private$stats_pvt <- value

            },

            reduce = function(value) {

              if(missing(value)) return(private$reduce_pvt)

              private$reduce_pvt <- value

            }

          )
  )

library(R6)
#' Layout_Mgr R6 Class
#'
#' @export
MultiYearStatsMgr <-
  R6Class(classname = "MultiYearStatsMgr",
          inherit = StatsMgr,


          ################################################################################
          ##
          ##           PRIVATE
          ##
          ################################################################################

          private = list(
            years_pvt = NULL,
            cois_pvt = NULL

          ),

          ################################################################################
          ##
          ##           PUBLIC
          ##
          ################################################################################

          public = list(
            initialize = function(years = NULL, cois = NULL, ...) {

              private$years_pvt <- years
              private$cois_pvt <- cois

              super$initialize(...)

            },


            survey_stats = function(years = NULL, cois = NULL, value = "Yes", stat = "percent", ... ){

              cois <- private$cois_pvt
              years <- private$years_pvt

              if(length(cois)==1) {

                cois <- rep(cois, length(years))

              }

              multi_attrs <-  list()

              df_stats <- purrr::map2(years, cois, function(year, coi) {

                private$data_mgr_pvt$params_mgr$set(year = year)

                df <- super$survey_stats(coi = coi, ...) %>%
                  mutate(year = {{year}})  %>%
                  select(year, subvar, subset, response, all_of(stat))

                multi_attrs <<- df %>% attributes()

                df
              }) %>% bind_rows


              df_stats <- df_stats %>%
                filter(grepl(value, response))

              response <- df_stats %>% pull(response) %>% tail(1)

              df_stats <- df_stats %>% mutate(response = as.character(year)) %>%
                select(-year)

              structure(df_stats,
                        class = c("brfss_stats", "data.frame"),
                        response = response,
                        label = attr(cois %>% tail(1), "label"),
                        years = years,
                        cois = cois,
                        geog = multi_attrs$geog ,
                        stat_cols = multi_attrs$stat_cols ,
                        population = multi_attrs$population ,
                        section_type = multi_attrs$section_type ,
                        section_num = multi_attrs$section_num ,
                        section_index = multi_attrs$section_index ,
                        section_name = multi_attrs$section_name ,
                        section = multi_attrs$section ,
                        question = multi_attrs$question ,
                        label = multi_attrs$label ,
                        weighted = multi_attrs$weighted ,
                        weight_col = multi_attrs$weight_col ,
                        conf = multi_attrs$conf

              )
            }

          ),

          ################################################################################
          ##
          ##           ACTIVE
          ##
          ################################################################################

          active = list(

            cois = function(value) {

              if(missing(value)) return(private$cois_pvt)

              private$cois_pvt <- value

            },

            years = function(value) {

              if(missing(value)) return(private$years_pvt)

              private$years_pvt <- value

            }

          )
  )
