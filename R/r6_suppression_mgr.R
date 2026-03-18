
library(R6)

#' SuppressionMgr R6 Class
#'
#' @export
#'
SuppressionMgr <-
  R6::R6Class(
    classname = "SuppressionMgr",


    private = list(

      suppressions_pvt = data.frame(),

      suppress_pvt = FALSE,
      suppress_if_pvt =  NULL,
      display_type_pvt = "remove",

      add_suppression = function(df, include_why = FALSE) {

        rule_exprs <- purrr::map(private$suppress_if_pvt, rlang::parse_expr)

        df$suppress_reason <- purrr::pmap_chr(
          df,
          function(...) {

            row <- list(...)
            hits <- names(rule_exprs)[
              purrr::map_lgl(rule_exprs, ~ rlang::eval_tidy(.x, data = row))
            ]
            if (length(hits) == 0) NA_character_ else paste(hits, collapse = "; ")
          }
        )

        df$suppress <- !is.na(df$suppress_reason)

        if(!include_why) df <- df %>% select(-suppress_reason)

        years <- attr(df_stats,"years") %>% as.character()
        cois <- attr(df_stats,"cois")
        names(cois) <- years

        sup_cois <- cois[as.character(df$year)]

        private$suppressions_pvt <- df  %>%
          mutate(coi = .env$sup_cois) %>%
          filter(suppress) %>%
          select(coi, year, subvar, subset)

        if(self$display_type == "remove") {

          df <- df %>% anti_join(private$suppressions_pvt,
                                 by = join_by(year, subvar, subset))
        }

        df
      }

    ),

    public = list(

      initialize = function(display_type = NULL) {

        if(!is.null(display_type)) {

          display_type <- match.arg(display_type, SuppressionMgr$display_types())

          private$display_type_pvt <- display_type
        }
      },

      reset = function() {

        private$suppressions_pvt = data.frame()

      },

      suppress = function(df_stats) {

        df_stats %>% private$add_suppression()
      }

    ),

    active = list(

      suppressions = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        private$suppressions_pvt
      },


      display_type = function(value) {

        if(!missing(value)) {
          private$display_type_pvt <- display_type
          return()
        }

        private$display_type_pvt
      }



    )

  )

#' DefaultSuppressionMgr R6 Class
#'
#' @export
#'
DefaultSuppressionMgr <-
  R6::R6Class(
    classname = "DefaultSuppressionMgr",
    inherit = SuppressionMgr,

    public = list(

      initialize = function(...) {

        private$suppress_if_pvt =  c(low_num = "num < 6", high_cv = "cv > 30")

        super$initialize(...)
      }
    )
  )

#' @export
SuppressionMgr$display_types <- function() {

  c("remove", "substitue", "style")
}
