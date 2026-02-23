
library(R6)
#' Layout_Mgr R6 Class
#'
#' @export
StatsMgr <-
  R6Class(
    classname = "StatsMgr",


    ################################################################################
    ##
    ##           PRIVATE
    ##
    ################################################################################

    private = list(
      data_mgr_pvt = NULL,
      years_pvt = NULL,
      cois_pvt = NULL,
      cols_req_pvt = c("subvar","subset","response"),
      stats_pvt = c("num","den",
                    "percent","se","CI_lower","CI_upper","cv",
                    "percent_unwtd","num_wtd","den_wtd"),
      subsets_pvt = NULL,
      subpopulation_pvt = NULL,
      responses_pvt = ".*",
      exclude_pvt = c("Don?t|Refuse"),
      sub_exclude_pvt = c("Don?t|Refuse"),
      conf_pvt =.95,
      weighted_pvt = TRUE,
      weight_col_pvt = "_LLCPWT",
      pct_pvt = TRUE,
      digits_pvt = 99,
      combine_ci_pvt = TRUE,
      my_stats_pvt = NULL,
      reduce_pvt = FALSE,
      subsets_only_pvt = FALSE,

      select_cols = function(df) {


        if(private$combine_ci_pvt && "ci" %in% colnames(df)) {
          df <- df %>%
            mutate(ci = paste0(CI_lower, "-", CI_upper)) %>%
            relocate(ci, .before = CI_lower) %>%
            select(-starts_with("CI_"))
        }

        if("ci" %in% private$my_stats_pvt) {
          stats_cmp <- c(private$my_stats_pvt, "CI_lower", "CI_upper")
        } else {
          stats_cmp <- private$my_stats_pvt
        }

        rm <- setdiff(private$stats_pvt, stats_cmp)

        df <- df %>% filter(response != "dummy") %>%
          select(-all_of(rm))

        df

      }

    ),

    ################################################################################
    ##
    ##           PUBLIC
    ##
    ################################################################################

    public = list(

      initialize = function(data_mgr = NULL,
                            years = NULL,
                            cois = "",
                            stats = NULL,
                            responses = ".*",
                            exclude = c("Don.*t|Refuse"),
                            subsets = NULL,
                            sub_exclude = c("Don.*t|Refuse"),
                            subpopulation = NULL,
                            conf =.95,
                            weighted = TRUE,
                            weight_col = "_LLCPWT",
                            pct = TRUE,
                            combine_ci = FALSE,
                            digits = 2,
                            reduce = FALSE,
                            subsets_only = FALSE) {


        if(!is.null(data_mgr)) {

          if(inherits(data_mgr, "DataMgr")) private$data_mgr_pvt <- data_mgr

        } else {

          private$data_mgr_pvt <- DataMgr$new()

        }


        private$cois_pvt <- cois
        private$years_pvt <- years
        private$responses_pvt <- responses
        private$exclude_pvt <- exclude
        private$subsets_pvt <-subsets
        private$subpopulation_pvt <- subpopulation
        private$sub_exclude_pvt <- sub_exclude
        private$conf_pvt <-conf
        private$weighted_pvt <- weighted
        private$weight_col_pvt <- weight_col
        private$pct_pvt <- pct
        private$combine_ci_pvt <- combine_ci
        private$digits_pvt <- digits
        private$reduce_pvt <- reduce
        private$subsets_only_pvt <-subsets_only

        if(is.null(stats))
          private$my_stats_pvt <- private$stats_pvt
        else
          private$my_stats_pvt <- stats

      },

      add_subset = function(subset) {

        self$subsets <- c(self$subsets, subset)

      },

      show_all_subs = function() {

        self$sub_exclude <- "^$"

      },

      show_all_responses = function() {

        self$responses <- ".*"
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

      # -----------------------------------------------------------------------------
      #
      #     suvey_stats

      survey_stats = function(years = NULL, cois = NULL, value = ".*", ... ){

        cois <-  cois %||% private$cois_pvt
        years <- years %||% private$years_pvt

        if(is.null(years)) years <- private$data_mgr_pvt$dataset_mgr$get(year)

        if(length(cois)==1) {

          cois <- rep(cois, length(years))

        }

        multi_attrs <-  list()

        df_stats <- purrr::map2(years, cois, function(year, coi) {

          private$data_mgr_pvt$dataset_mgr$set(year = year)

          df <- self$survey_stats_one(coi = coi, ...)

          df <- df %>%
            mutate(year = .env$year)

          multi_attrs <<- df %>% attributes()

          df
        }) %>% bind_rows() %>%
          relocate(year)

        response <- df_stats %>% pull(response) %>% unique()

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
      },

      survey_stats_one = function(coi = NULL, weighted = NULL, subsets = NULL,
                                  pct = NULL, digits = NULL, subsets_only = NULL,
                                  reduce = NULL, combine_ci = NULL, wide = FALSE) {

        pvt <- private

        cois <- coi %||% pvt$cois_pvt
        pct <- pct %||% pvt$pct_pvt
        digits <- digits %||% pvt$digits_pvt
        subsets <- subsets %||% pvt$subsets_pvt
        subsets_only <- subsets_only %||% pvt$subsets_only_pvt

        if(is.null(coi) || nchar(coi) == 0 || length(coi) == 0) {
          message("must select a valid column of interest (coi)")
          return(NULL)
        }

        weighted <- ifelse(is.null(weighted), pvt$weighted_pvt,weighted)
        reduce <- ifelse(is.null(reduce), pvt$reduce_pvt,reduce)

        if(!is.null(pvt$data_mgr_pvt)) data <- pvt$data_mgr_pvt$prepped_data

        df <- survey_stats(
          df_data = data,
          coi = coi,
          exclude = pvt$exclude_pvt,
          subsets = subsets,
          subset_by = pvt$subset_by_pvt,
          sub_exclude = pvt$sub_exclude_pvt,
          conf = pvt$conf_pvt,
          weighted = weighted,
          weight_col = pvt$weight_col_pvt,
          pct = pct,
          digits = digits)

        if(reduce) {

          df <- df %>%
            select(-any_of(c("num", "den", "percent_unwtd"))) %>%
            rename_with(.fn = ~gsub("_wtd","", .x))
        }

        df <- df %>% private$select_cols()
        df <- df %>% filter(grepl(self$responses, response))

        if(wide) df <- self$widen(df)

        if(subsets_only) {
          df <- df %>% filter(subvar != "")
        }

        return(df)

      },

      widen = function(df_stats = NULL, coi = NULL, sep_char = "^") {

        stats <- StatsMgr$stats_names() %>%
          paste0(., collapse = "$|^") %>%
          paste0("^", ., "$")

        if(is.null(df_stats)) {

          df_stats <- self$survey_stats(coi)

        }


        df_wide <- df_stats %>%
          tidyr::pivot_wider(names_from = c(matches("response|year")),
                             id_cols = c(subvar, subset),
                             values_from =  c(matches(stats)), names_vary = "slowest",
                             names_sep = "^") %>%
          as.data.frame()

        df_wide

      }

    ),

    ################################################################################
    ##
    ##           ACTIVE
    ##
    ################################################################################

    active = list(

      year = function(value) {

        if(missing(value)) {
          return(self$data_mgr$dataset_mgr$get(year))
        } else {
          if(is.numeric(value)) {
            self$data_mgr$dataset_mgr$set(year = year)
          }
        }

      },

      data_mgr = function(value) {

        if(!missing(value)) {

          if(inherits(value, "DataMgr")) private$data_mgr_pvt <- data_mgr

        } else {

          return(private$data_mgr_pvt)

        }

      },
      #
      # survey_data = function(value) {
      #
      #   if(missing(value)) return(private$data_pvt)
      #
      #   private$data_pvt <- value
      #
      # },

      coi = function(value) {

        if(missing(value)) return(private$cois_pvt)

        private$cois_pvt <- value

      },

      cois = function(value) {

        if(missing(value)) return(private$cois_pvt)

        private$cois_pvt <- value

      },

      years = function(value) {

        if(missing(value)) return(private$years_pvt)

        private$years_pvt <- value

      },

      responses = function(value) {

        if(missing(value)) return(private$responses_pvt)

        private$responses_pvt <- value

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

      subsets_only = function(value) {

        if(missing(value)) return(private$subsets_only_pvt)

        private$subsets_only_pvt <- value

      },

      digits = function(value) {

        if(missing(value)) return(private$digits_pvt)

        private$digits_pvt <- value

      },

      combine_ci = function(value) {

        if(missing(value)) return(private$combine_ci_pvt)

        if(!is.logical(value)) {
          message("This property requires a logical value")
          return(NULL)

        }
        private$combine_ci_pvt <- value

      },



      stats = function(value) {

        if(missing(value)) return(private$my_stats_pvt)

        private$my_stats_pvt <- value

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


            survey_stats = function(years = NULL, cois = NULL, value = ".*", ... ){

              cois <- private$cois_pvt
              years <- private$years_pvt

              if(length(cois)==1) {

                cois <- rep(cois, length(years))

              }

              multi_attrs <-  list()

              df_stats <- purrr::map2(years, cois, function(year, coi) {

                private$data_mgr_pvt$dataset_mgr$set(year = year)

                df <- super$survey_stats(coi = coi, ...)

                df <- df %>%
                  mutate(year = .env$year)

                multi_attrs <<- df %>% attributes()

                df
              }) %>% bind_rows() %>%
                relocate(year)

              response <- df_stats %>% pull(response) %>% unique()

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

            coi = function(value) {

              if(missing(value)) return(private$cois_pvt)

              private$cois_pvt <- value

            },

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

#' @export
StatsMgr$stats_names <- function() {

  c("num", "den", "percent", "se", "ci","CI_lower", "CI_upper", "cv")
}
