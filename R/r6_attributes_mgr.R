
library(R6)
#' Attributes_Mgr R6 Class
#'
#' @export
Attributes_Mgr <-
  R6Class(classname = "Attributes_Mgr",

          public = list(

            # ============================================

            initialize = function(lst= NULL) {
              if(!is.null(lst)) self$add_list(lst)
            },

            # ============================================

            add_list = function(lst) {

              my_attrs <- private$atts %>% names()
              purrr::map2(names(lst), lst, function(nm,val) {

                if(nm %in% my_attrs) {
                  private$atts[[nm]] <<- val

                }
              })

              return(invisible(self))   # returns self invisibly
            },

            # ============================================

            as.list = function() {
              private$atts
            },

            # ============================================

            req_atts = function() {
              private$atts %>% names()
            } ,

            # ============================================

            missing = function(x) {

              x_attrs <- attributes(x) %>% names()
              my_attrs <- private$atts %>% names()

              my_attrs[!my_attrs %in% x_attrs]
            },

            # =========== add these attributes to an object  =========

            load = function(x) {

              x_attrs <- attributes(x)
              my_attrs <- private$atts %>% names()

              purrr::map2(names(x_attrs), x_attrs, function(nm,val) {
                if(nm %in% my_attrs)  self[[nm]] <- val
              })

              return(invisible(self))   # returns self invisibly
            },

            # =========== make this object a factor  =========

            factorize = function(x, ...) {

              old_attribs <- attributes(x)
              new_attribs <- private$atts

              x1 <- factor(x, ...)
              class(x1) <- c("brfss", "factor")

              x <- self$modify(x1)


              return(invisible(x))   # returns x invisibly
            },

            # =========== add these attributes to an object  =========

            modify = function(x) {

              all_attribs <- private$merge_atts(attributes(x))

              attributes(x) <- all_attribs
              x
            },

            # -----------  helper functions  ----------------

            is.custom = function(val) {

              if(missing(val)) {
                private$atts$is_custom = TRUE
                private$atts$is_calculated = TRUE

              } else private$atts$is_custom = val

              return(invisible(self))   # returns self invisibly
            },

            is.calculated = function(val) {

              if(missing(val)) {
                private$atts$is_calculated = TRUE

              } else private$atts$is_calculated = val

              return(invisible(self))   # returns self invisibly
            },

            is.saq = function() {
              private$atts$is_saq = TRUE

              return(invisible(self))   # returns self invisibly
            }
          ),

          #==============================================================
          #
          #         private
          #
          private = list(
            atts = list(
              section_type = "Core",
              section_num = "",
              section_index = "",
              section_name = "",
              label = "",
              question = "",
              variable = "",
              population = "All Respondents",
              is_calculated = FALSE,
              is_custom = FALSE,
              is_saq = FALSE),

            merge_atts = function(old_attribs) {

              new_attribs <- private$atts

              old_names <- names(old_attribs)
              new_names <- names(new_attribs)
              keep <- setdiff(old_names, new_names)

              c(old_attribs[keep], new_attribs)
            }


          ),

          #==============================================================
          #
          #         active
          #
          active = list(

            # ====================  section_type  ========================
            #
            section_type = function(value) {
              if (missing(value)) {
                private$atts$section_type     # when reading
              } else {
                if(!is.character(value)) {
                  warning("section_type class must be character")
                  return(NULL)
                }
                if(!value %in% c("Core", "Module", "Non-Survey")) {
                  warning("section_type class must be character")
                  return(NULL)
                }
                private$atts$section_type <- value  # when assigning
              }
            },

            # ====================  population  ========================
            #
            population = function(value) {
              if (missing(value)) {
                private$atts$population     # when reading
              } else {
                if(!is.character(value)) {
                  warning("population class must be character")
                  return(NULL)
                }

                private$atts$population <- value  # when assigning
              }
            },

            # ====================  section_num  ========================
            #
            section_num = function(value) {
              if (missing(value)) {
                private$atts$section_num     # when reading
              } else {
                private$atts$section_num <- value  # when assigning
              }
            },

            # ====================  section_index  ========================
            #
            section_index = function(value) {
              if (missing(value)) {
                private$atts$section_index     # when reading
              } else {
                private$atts$section_index <- value  # when assigning
              }
            },

            # ====================  section_name  ========================
            #
            section_name = function(value) {
              if (missing(value)) {
                private$atts$section     # when reading
              } else {
                if(!is.character(value)) {
                  warning("section_name class must be character")
                  return(NULL)
                }
                private$atts$section_name <- value  # when assigning
              }
            },

            # ====================  label  ========================
            #
            label = function(value) {
              if (missing(value)) {
                private$atts$label     # when reading
              } else {
                private$atts$label <- value  # when assigning
              }
            },

            # ====================  question  ========================
            #
            question = function(value) {
              if (missing(value)) {
                private$atts$question     # when reading
              } else {
                private$atts$question <- value  # when assigning
              }
            },

            # ====================  variable  ========================
            #
            variable = function(value) {
              if (missing(value)) {
                private$atts$variable     # when reading
              } else {
                private$atts$variable <- value  # when assigning
              }
            },

            # ====================  coi, alias for variable  ========================
            #
            coi = function(value) {
              if (missing(value)) {
                private$atts$variable     # when reading
              } else {
                private$atts$variable <- value  # when assigning
              }
            },

            # ====================  column, alias for variable  ========================
            #
            column = function(value) {
              if (missing(value)) {
                private$atts$variable     # when reading
              } else {
                private$atts$variable <- value  # when assigning
              }
            },

            # ====================  is_calculated  ========================
            #
            is_calculated = function(value) {
              if (missing(value)) {
                private$atts$is_calculated     # when reading
              } else {
                if(!inherits(try(as.logical(x), silent = TRUE), "try-error")) {
                  private$atts$is_calculated <- as.logical(value)  # when assigning
                } else {
                  private$atts$is_calculated <- FALSE
                  warning("Invalid value ... is_calculated set to FALSE")
                }
              }
            },

            # ====================  is_custom  ========================
            #
            is_custom = function(value) {
              if (missing(value)) {
                private$atts$is_custom     # when reading
              } else {
                if(!inherits(try(as.logical(x), silent = TRUE), "try-error")) {
                  private$atts$is_custom <- as.logical(value)  # when assigning
                } else {
                  private$atts$is_custom <- FALSE
                  warning("Invalid value ... is_custom set to FALSE")
                }
              }
            },

            # ====================  is_saq  ========================
            #
            is_saq = function(value) {
              if (missing(value)) {
                private$atts$is_saq     # when reading
              } else {
                if(!inherits(try(as.logical(x), silent = TRUE), "try-error")) {
                  private$atts$is_saq <- as.logical(value)  # when assigning
                } else {
                  private$atts$is_saq <- FALSE
                  warning("Invalid value ... is_saq set to FALSE")
                }
              }
            }

          )
  )

# is_calculated = FALSE,
# is_custom = FALSE,
# is_saq = FALSE)


