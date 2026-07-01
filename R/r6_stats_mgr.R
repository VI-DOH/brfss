
#' StatsMgr R6 Class
#'
#' @export
StatsMgr <-
  R6::R6Class(
    classname = "StatsMgr",


    ################################################################################
    ##
    ##           PRIVATE
    ##
    ################################################################################

    private = list(
      ..data_mgr = NULL,
      ..suppression_mgr = NULL,
      ..years = NULL,
      ..cois = NULL,
      ..cols_req = c("subvar","subset","response"),
      ..stats = c("num","den",
                  "percent","se","CI_lower","CI_upper","rse",
                  "percent_unwtd","num_wtd","den_wtd"),
      ..subvars = NULL,
      ..subpopulation = NULL,
      ..responses = ".*",
      ..exclude = c("Don?t|Refuse"),
      ..sub_exclude = c("Don?t|Refuse"),
      ..conf =.95,
      ..weighted = TRUE,
      ..weight_col = "_LLCPWT",
      ..pct = TRUE,
      ..digits = 99,
      ..combine_ci = TRUE,
      ..my_stats = NULL,
      ..reduce = FALSE,
      ..subvars_only = FALSE,

      # suppression objects

      ..suppress = FALSE,
      ..suppress_if = NULL,

      ..select_cols = function(df) {

        if(private$..combine_ci && "ci" %in% colnames(df)) {
          df <- df %>%
            mutate(ci = paste0(CI_lower, "-", CI_upper)) %>%
            relocate(ci, .before = CI_lower) %>%
            select(-starts_with("CI_"))
        }

        if("ci" %in% private$..my_stats) {
          stats_cmp <- c(private$..my_stats, "CI_lower", "CI_upper")
        } else {
          stats_cmp <- private$..my_stats
        }

        rm <- setdiff(private$..stats, stats_cmp)

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

      initialize = function(data_mgr = NULL, suppression_mgr = NULL,
                            years = NULL,
                            cois = "",
                            stats = "percent",
                            responses = ".*",
                            exclude = c("Don.*t|Refuse"),
                            subvars = NULL,
                            sub_exclude = c("Don.*t|Refuse"),
                            subpopulation = NULL,
                            conf =.95,
                            weighted = TRUE,
                            weight_col = "_LLCPWT",
                            pct = TRUE,
                            combine_ci = FALSE,
                            digits = 2,
                            reduce = FALSE,
                            subvars_only = FALSE,
                            suppress = FALSE) {


        if(!is.null(data_mgr)) {

          if(inherits(data_mgr, "DataMgr")) private$..data_mgr <- data_mgr

        } else {

          private$..data_mgr <- DataMgr$new()

        }

        if(!is.null(suppression_mgr)) {

          if(inherits(suppression_mgr, "SuppressionMgr"))
            private$..suppression_mgr <- suppression_mgr

        } else {

          private$..suppression_mgr <- DefaultSuppressionMgr$new()

        }


        private$..cois <- cois
        private$..years <- years
        private$..responses <- responses
        private$..exclude <- exclude
        private$..subvars <-subvars
        private$..subpopulation <- subpopulation
        private$..sub_exclude <- sub_exclude
        private$..conf <-conf
        private$..weighted <- weighted
        private$..weight_col <- weight_col
        private$..pct <- pct
        private$..combine_ci <- combine_ci
        private$..digits <- digits
        private$..reduce <- reduce
        private$..subvars_only <-subvars_only
        private$..suppress <- suppress

        if(is.null(stats))
          private$..my_stats <- private$..stats
        else
          private$..my_stats <- stats

      },

      add_subvar = function(subvar) {

        self$subvars <- c(self$subvars, subvar)

      },

      show_all_subs = function() {

        self$sub_exclude <- NULL

      },

      show_all_responses = function() {

        self$responses <- ".*"
        self$exclude <- "^$"

      },

      remove_stats = function(pttrn) {

        private$..my_stats <-
          private$..my_stats %>%
          grep(pttrn, ., value = T, invert = T)
      },

      reset_stats = function() {

        private$..my_stats <- private$..stats
      },

      # -----------------------------------------------------------------------------
      #
      #     suvey_stats

      survey_stats = function(years = NULL, cois = NULL, suppress = NULL, expand = FALSE,
                              value = ".*", wide = FALSE, ... ){

        cois <-  cois %||% private$..cois
        years <- years %||% private$..years
        suppress <- suppress %||% private$..suppress

        if(is.null(years)) years <- private$..data_mgr$dataset_mgr$get(year)

        if(length(cois)==1) {

          cois <- rep(cois, length(years))

        }

        multi_attrs <-  list()

        # -------------------------------------------------------------------
        #
        #         cycle through the years

        df_stats <- purrr::map2(years, cois, function(year, coi) {

          private$..data_mgr$dataset_mgr$set(year = year)

          if(!private$..data_mgr$has_data)  {
            return(NULL)
          }

          df <- self$survey_stats_one_year(coi = coi, ...)

          if(is.null(df)) {
            return(NULL)
          }

          df <- df %>%
            mutate(year = .env$year)

          multi_attrs <<- df %>% attributes()

          df
        }) %>% bind_rows()

        if(is.null(df_stats) || nrow(df_stats) == 0) return(NULL)

        df_stats <- df_stats%>%
          relocate(year)

        if(suppress) {
          #df_stats <- df_stats %>% private$..suppression_mgr$suppress()
        }

        if(!expand) df_stats <- df_stats %>% private$..select_cols()

        response <- df_stats %>% pull(response) %>% unique()

        if(wide) {

          df_stats <- self$widen(df_stats)

        }

        years_txt  <-  if(length(years) > 0) paste0(min(years),"-", max(years)) else as.character(years)

        structure(df_stats,
                  class = c("brfss_stats", "data.frame"),
                  response = response,
                  label = attr(cois %>% tail(1), "label"),
                  years = years,
                  cois = cois,
                  years_txt = years_txt,
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

      survey_stats_one_year = function(df_data = NULL, coi = NULL, weighted = NULL, subvars = NULL,
                                       pct = NULL, digits = NULL, subvars_only = NULL,
                                       reduce = NULL, combine_ci = NULL) {


        pvt <- private

        cois <- coi %||% pvt$..cois
        pct <- pct %||% pvt$..pct
        digits <- digits %||% pvt$..digits

        if(length(subvars) == 1 && is.na(subvars)) {
          subvars  <-  NULL
          subvars_only <- FALSE
        } else {
          subvars <- subvars %||% pvt$..subvars
          subvars_only <- subvars_only %||% pvt$..subvars_only
        }


        if(is.null(coi) || nchar(coi) == 0 || length(coi) == 0) {
          message("must select a valid column of interest (coi)")
          return(NULL)
        }

        weighted <- ifelse(is.null(weighted), pvt$..weighted,weighted)
        reduce <- ifelse(is.null(reduce), pvt$..reduce,reduce)

        if(is.null(df_data)) {
          if(!is.null(pvt$..data_mgr)) data <- pvt$..data_mgr$prepped_data
        } else {
          data <- df_data
        }

        df <- survey_stats(
          df_data = data,
          coi = coi,
          exclude = pvt$..exclude,
          subvars = subvars,
          subset_by = pvt$..subset_by,
          sub_exclude = pvt$..sub_exclude,
          conf = pvt$..conf,
          weighted = weighted,
          weight_col = pvt$..weight_col,
          pct = pct,
          digits = digits)


        if(is.null(df)) return(NULL)

        if(reduce) {

          df <- df %>%
            select(-any_of(c("num", "den", "percent_unwtd"))) %>%
            rename_with(.fn = ~gsub("_wtd","", .x))
        }


        df <- df %>% filter(grepl(self$responses, response))

        if(subvars_only) {
          df <- df %>% filter(subvar != "")
        }

        return(df)

      },

      rolling_rates = function(years, col, subvars = NULL, verbose = FALSE) {

        yr0 <- head(years, 1)
        yr1 <- tail(years, 1)

        if(yr0 == yr1) {

          message("This method is for multiple year calculations. Only 1 year was provided")
          return(NULL)

        }

        dm <- private$..data_mgr

        df <- dm$combine_years(years = years, col = col, subvars = subvars)

        wc_ret <- private$..weight_col
        private$..weight_col <- "FINAL_WT"

        df_stats <- self$survey_stats_one_year(df_data = df, coi = col,
                                               subvars = subvars, pct = TRUE, digits = 1) %>%
          select(subvar,subset, response, num, den, percent, se, starts_with("CI"), rse)


        attr(df_stats, "year") = paste0(yr0, "-", yr1)

        private$..weight_col <-wc_ret

        df_stats
      },

      suppress = function(df) {

        df_supp <- df %>%
          filter(is.na(rse) | rse > 30.0) %>%
          select(year, subvar, subset) %>%
          mutate(supp = TRUE) %>%
          distinct()

        if(nrow(df_supp) > 0) {
          df <- df %>%
            left_join(df_supp, by = join_by(year, subvar, subset)) %>%
            mutate(supp = replace(supp, is.na(supp), FALSE)) %>%
            mutate(across(num:last_col(), ~ if_else(supp, NA, .x))) %>%
            select(-supp)
        }

        df
      },

      widen = function(df_stats = NULL, coi = NULL, sep_char = "^") {

        stats <- StatsMgr$stats_names() %>%
          setdiff("den") %>%
          paste0(., collapse = "$|^") %>%
          paste0("^", ., "$")

        if(is.null(df_stats)) {

          df_stats <- self$survey_stats(coi)

        }

        df_wide <- df_stats %>%
          tidyr::pivot_wider(names_from = c(matches("response|year")),
                             id_cols = any_of(c("subvar", "subset", "den")),
                             values_from =  c(matches(stats)), names_vary = "slowest",
                             names_sep = sep_char) %>%
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

          if(inherits(value, "DataMgr")) private$..data_mgr <- value

        } else {

          return(private$..data_mgr)

        }

      },

      suppression_mgr = function(value) {

        if(missing(value)) return(private$..suppression_mgr)

        if(inherits(value, "SuppressionMgr")) {
          private$..suppression_mgr <- value
        } else {
          message("value ust be a SuppressionMgr object")

        }
      },

      #
      # survey_data = function(value) {
      #
      #   if(missing(value)) return(private$..data)
      #
      #   private$..data <- value
      #
      # },

      coi = function(value) {

        if(missing(value)) return(private$..cois)

        private$..cois <- value

      },

      cois = function(value) {

        if(missing(value)) return(private$..cois)

        private$..cois <- value

      },

      years = function(value) {

        if(missing(value)) return(private$..years)

        private$..years <- value

      },

      responses = function(value) {

        if(missing(value)) return(private$..responses)

        private$..responses <- value

      },

      exclude = function(value) {

        if(missing(value)) return(private$..exclude)

        private$..exclude <- value

      },

      subvars = function(value) {

        if(missing(value)) return(private$..subvars)

        private$..subvars <- value

      },

      subpopulation = function(value) {

        if(missing(value)) return(private$..subpopulation)

        private$..subpopulation <- value

      },

      sub_exclude =function(value) {

        if(missing(value)) return(private$..sub_exclude)

        private$..sub_exclude <- value

      },

      conf =function(value) {

        if(missing(value)) return(private$..conf)

        private$..conf <- value

      },

      weighted = function(value) {

        if(missing(value)) return(private$..weighted)

        private$..weighted <- value

      },

      weight_col = function(value) {

        if(missing(value)) return(private$..weight_col)

        private$..weight_col <- value

      },

      pct = function(value) {

        if(missing(value)) return(private$..pct)

        private$..pct <- value

      },

      subvars_only = function(value) {

        if(missing(value)) return(private$..subvars_only)

        private$..subvars_only <- value

      },

      digits = function(value) {

        if(missing(value)) return(private$..digits)

        private$..digits <- value

      },

      combine_ci = function(value) {

        if(missing(value)) return(private$..combine_ci)

        if(!is.logical(value)) {
          message("This property requires a logical value")
          return(NULL)

        }
        private$..combine_ci <- value

      },

      # suppress = function(value) {
      #
      #   if(missing(value)) return(private$..suppress)
      #
      #   if(!is.logical(value)) {
      #     message("This property requires a logical value")
      #     return(NULL)
      #
      #   }
      #   private$..suppress <- value
      #
      # },



      stats = function(value) {

        if(missing(value)) return(private$..my_stats)

        private$..my_stats <- value

      },

      reduce = function(value) {

        if(missing(value)) return(private$..reduce)

        private$..reduce <- value

      }

    )
  )

#' Layout_Mgr R6 Class
#'
#' @export
MultiYearStatsMgr <-
  R6::R6Class(classname = "MultiYearStatsMgr",
              inherit = StatsMgr,


              ################################################################################
              ##
              ##           PRIVATE
              ##
              ################################################################################

              private = list(
                ..years = NULL,
                ..cois = NULL

              ),

              ################################################################################
              ##
              ##           PUBLIC
              ##
              ################################################################################

              public = list(
                initialize = function(years = NULL, cois = NULL, ...) {

                  private$..years <- years
                  private$..cois <- cois

                  super$initialize(...)

                },


                survey_stats = function(years = NULL, cois = NULL, value = ".*", ... ){

                  cois <- private$..cois
                  years <- private$..years

                  if(length(cois)==1) {

                    cois <- rep(cois, length(years))

                  }

                  multi_attrs <-  list()

                  df_stats <- purrr::map2(years, cois, function(year, coi) {

                    private$..data_mgr$dataset_mgr$set(year = year)

                    df <- super$survey_stats(coi = coi, ...)

                    df <- df %>%
                      mutate(year = .env$year)

                    multi_attrs <<- df %>% attributes()

                    df
                  }) %>% bind_rows()

                  if(is.null(df_stats)) return(NULL)

                  df_stats <- df_stats %>%
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

                  if(missing(value)) return(private$..cois)

                  private$..cois <- value

                },

                cois = function(value) {

                  if(missing(value)) return(private$..cois)

                  private$..cois <- value

                },

                years = function(value) {

                  if(missing(value)) return(private$..years)

                  private$..years <- value

                }

              )
  )

#' @export
StatsMgr$stats_names <- function() {

  c("num", "den", "percent", "se", "ci","CI_lower", "CI_upper", "rse")
}
