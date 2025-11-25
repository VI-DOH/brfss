
library(R6)
#' Layout_Mgr R6 Class
#'
#' @export
Layout_Mgr <-
  R6Class(classname = "Layout_Mgr",
          public = list(

            year = NULL,

            # =======================================================================

            initialize = function(year = NULL, type = c("merged", "codebook", "sas")) {

              type <- match.arg(type,  c("merged", "codebook", "sas"))

              if (!requireNamespace("dplyr", quietly = TRUE)) {
                stop("Package 'dplyr' is required but not installed.")
              }

              if (!requireNamespace("brfss", quietly = TRUE)) {
                stop("Package 'brfss' is required but not installed.")
              }

              # --- get the year in case it isn't the current working year

              if(!is.null(year)) {
                old_year <- year
                brfss.params(year = year)

              } else {
                old_year <- brfss.param(year)
              }

              self$year <- old_year

              # --------------  create the func name  ---------------

              func  <-  paste0("get.", type,  ".layout")

              # -------------  get the layout  ---------------

              df  <-  do.call(getExportedValue("brfss", func), args = list())|>
                dplyr::mutate(across(
                  where(is.character),
                  ~ gsub("\u00A0", " ", .x)  # non-breaking space → regular space
                ))

              # --- reset the year in case it wasn't the current working year

              brfss.params(year = old_year)

              # --------   clean up the data.frame  -----------------

              max_mod <- df %>% filter(sect_type == "Module") %>%
                pull(sect_num) %>% max()

              last_max <- df %>% pull(sect_num) %>% {which(. == max_mod)} %>%
                max()

              df <- df %>% mutate(rn = row_number()) %>%
                mutate(sect_num =
                         if_else(rn > last_max & sect_type == "Module" & sect_num == 1,
                                 0, sect_num)) %>%
                select(-rn)

              # ---------  save the data.frame   --------------------

              private$df_layout <- df
            },

            # =======================================================================

            find_section = function(section = "", sect_type = "", sect_num = NULL,
                                    list = FALSE) {

              ret <- private$df_layout_data
              if(is.null(ret)) ret <- private$df_layout

              ret <- private$df_layout %>%
                filter(sect_type %in% c("Core", "Module", "SAQ"))

              sect <- ifelse(is.null(section),"", section)
              sec_typ <- ifelse(is.null(sect_type),"", sect_type)

              ret <- ret %>%
                filter(grepl({{sect}},section))  %>%
                filter(grepl({{sec_typ}},sect_type))

              if(!is.null(sect_num)) {
                ret <- ret %>%
                  filter(sect_num == {{sect_num}})
              }

              ret <- ret %>%
                group_by(section, sect_type, sect_num) %>%
                summarise(nquestions = n(), .groups = "drop") %>%
                as.data.frame()


              if(list) {
                ret <- lapply(seq_len(nrow(ret)), function(i) as.list(ret[i, ]))
                if(length(ret) == 1) ret <- ret[[1]]
              }

              ret

            },

            # =======================================================================

            section_questions = function(section = "", sect_type = "",
                                         sect_num = NULL,
                                         list = FALSE,
                                         mult_ok = FALSE) {

              ret <- private$df_layout_data
              if(is.null(ret)) ret <- private$df_layout

              sect <- section
              sec_typ <- sect_type

              ret <- ret %>%
                filter(grepl({{sect}},section))  %>%
                filter(grepl({{sec_typ}},sect_type))

              if(!is.null(sect_num)) {
                ret <- ret %>%
                  filter(sect_num == {{sect_num}})
              }

              ret <- ret %>%
                select(col_name, section, sect_type, sect_num,
                       question_num, any_of("saq"), question) %>%
                arrange(question_num)

              if(!mult_ok) {
                matches <- ret %>% pull(section) %>% unique()
                if(length(matches) > 1) {
                  matches <- paste0(matches, collapse = ", ")
                  warning(paste0("More than 1 section matches your section pattern",
                                 "\n ... ", matches))
                  return(NULL)
                }
              }

              if(list) {
                ret <- lapply(seq_len(nrow(ret)), function(i) as.list(ret[i, ]))
                if(length(ret) == 1) ret <- ret[[1]]
              }

              ret

            },

            # =======================================================================

            section_col_names = function(section = "", sect_type = "",
                                         sect_num = NULL) {

              df <- self$section_questions(section = section, sect_type = sect_type,
                                           sect_num = sect_num,
                                           list = FALSE,
                                           mult_ok = FALSE)

              df %>% pull(col_name)

            },

            # =======================================================================

            find_question = function(quest = "", sect_type = "",
                                     list = FALSE,
                                     mult_ok = FALSE) {

              ret <- private$df_layout
              sec_typ <- sect_type
              questn <- quest

              ret <- ret %>%
                filter(grepl({{sec_typ}},sect_type)) %>%
                filter(grepl({{questn}}, question))


              ret <- ret %>%
                select(col_name, section, sect_type, sect_num,
                       question_num, any_of("saq"), question)


              if(list) {
                ret <- lapply(seq_len(nrow(ret)), function(i) as.list(ret[i, ]))
                if(length(ret) == 1) ret <- ret[[1]]
              }

              ret

            },

            # =======================================================================

            sections = function(sect_type = "") {

              if(sect_type != "")
                sect_type = match.arg(sect_type, c("Core", "Module", ""))

              private$df_layout  %>%
                filter(grepl({{sect_type}},sect_type))%>%
                select(section, sect_type, sect_num) %>%
                filter(sect_type %in% c("Core", "Module")) %>%
                distinct() %>%
                filter(sect_num > 0)


            },

            # =======================================================================

            layout = function(basic = TRUE  ) {
              df <- private$df_layout

              if(basic) df <- df %>%
                  select(col_name,section_type,section_num, section_name,
                         label,question,
                         section_index,any_of("calculated"))

              df
            },

            # =======================================================================
            data_layout = function(basic = TRUE  ) {
              df <- private$df_layout_data

              if(basic) df <- df %>%
                  select(col_name,sect_type,sect_num, section,label,question,
                         question_num, any_of(c("calculated", "saq")))

              df
            },

            # =======================================================================

            layout_from_data = function(df = NULL) {

              if(is.null(df)) df <- prepped_data()

              col_names  <-  df %>% colnames()

              df_lo <- data.frame(variable = col_names, section_type = NA,
                                  section_num = NA,
                                  section_index = NA, section_name = NA,
                                  label = NA, question = NA)

              atts <- colnames(df_lo)

              for (i in 1:ncol(df)) {

                col <- df[[i]]
                attrx <- attributes(col) %>% names() %>% {.[!. %in% c("variable")]}

                sapply(atts, function(att) {

                  if(att %in% attrx) {

                    df_lo[i,att] <<- attr(col,att)
                  }

                })
              }


              private$df_layout_data <- df_lo %>%

                rename(col_name = variable,
                       section = section_name,
                       question_num = section_index) %>%

                filter(section_type %in% c("Core","Module","SAQ")) %>%
                inner_join(self$sections() %>% select(section, sect_type, sect_num),
                           by = join_by(section)) %>%
                select(-c(section_type, section_num))

            }

          ),

          # =======================================================================
          # =======================================================================

          private = list(
            df_layout = NULL,
            df_layout_data = NULL

          ),

          active = list(
          )
  )
