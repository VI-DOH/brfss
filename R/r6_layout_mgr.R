
library(R6)

#' Layout_Mgr R6 Class
#'
#' @export
Layout_Mgr <-
  R6Class(
    classname = "Layout_Mgr",

    private = list(
      year_pvt = NULL,
      df_layout_pvt = NULL,
      type_pvt = "merged",
      dataset_mgr_pvt = NULL,
      file_mgr_pvt = NULL,

      prep_layout = function(df) {

        #df  <-  do.call(self[[func]], args = list())

        df <- df %>%
          dplyr::mutate(across(
            where(is.character),
            ~ gsub("\u00A0", " ", .x)  # non-breaking space → regular space
          )) %>%
          filter(!grepl("^DUMMY", sect_type)) %>%
          filter(!is.na(sect_type)) %>%
          mutate(sect_num = as.integer(sect_num))

        # --------   clean up the data.frame  -----------------

        max_mod <- df %>%
          mutate(sect_num = as.integer(sect_num))%>%
          filter(sect_type == "Module") %>%
          pull(sect_num) %>% max()

        last_max <- df %>% pull(sect_num) %>% {which(. == max_mod)} %>%
          max()

        df <- df %>% mutate(rn = row_number()) %>%
          mutate(sect_num =
                   if_else(rn > last_max & sect_type == "Module" & sect_num == 1,
                           0, sect_num)) %>%
          select(-rn)

        # ---------  save the data.frame   --------------------

        private$df_layout_pvt <- df
      }

    ),

    public = list(

      # =======================================================================

      initialize = function(year = NULL,
                            type = NULL) {


        private$dataset_mgr_pvt <- DataSetMgr$new()$clone()

        private$file_mgr_pvt <- FileMgr$new(dataset_mgr = private$dataset_mgr_pvt)

        if(is.null(type))
          type <- self$get_best_type()
        else
          type <- match.arg(type,  c("merged", "codebook", "sas", "data", "saq"))

        if (!requireNamespace("dplyr", quietly = TRUE)) {
          stop("Package 'dplyr' is required but not installed.")
        }

        # --- get the year in case it isn't the current working year

        old_year <- private$dataset_mgr_pvt$get(year)

        if(is.null(year)) {
          private$year_pvt <- old_year
        } else {
          private$year_pvt <- year
          private$dataset_mgr_pvt$set(year = year)
        }

        private$type_pvt <- type

        private$df_layout_pvt <- self$get_layout(type)
      },

      # =======================================================================

      load_layout = function() {


      },



      find_section = function(section = "", sect_type = "", sect_num = NULL,
                              list = FALSE) {


        ret <- private$df_layout_pvt %>%
          filter(sect_type %in% c("Core", "Module", "SAQ"))

        sect <- ifelse(is.null(section),"", section)
        sec_typ <- ifelse(is.null(sect_type),"", sect_type)

        ret <- ret %>%
          filter(grepl(.env$sect,section))  %>%
          filter(grepl(.env$sec_typ,sect_type))

        if(!is.null(sect_num)) {
          ret <- ret %>%
            filter(sect_num == .env$sect_num)
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

        ret <- private$df_layout_pvt

        sect <- section
        sec_typ <- sect_type

        ret <- ret %>%
          filter(grepl(.env$sect,section))  %>%
          filter(grepl(.env$sec_typ,sect_type))

        if(!is.null(sect_num)) {
          ret <- ret %>%
            filter(sect_num == .env$sect_num)
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

        ret <- private$df_layout_pvt
        sec_typ <- sect_type
        questn <- quest

        ret <- ret %>%
          filter(grepl(.env$sec_typ,sect_type)) %>%
          filter(grepl(.env$questn, question), ignore.case = TRUE)


        ret <- ret %>%
          select(col_name, section, sect_type, sect_num,
                 question_num, any_of("saq"), question) %>%
          arrange(sect_type, sect_num,
                  question_num)


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

        tryCatch({
          private$df_layout_pvt  %>%
            filter(grepl(.env$sect_type,sect_type))%>%
            select(any_of("year"),section, sect_type, sect_num) %>%
            filter(sect_type %in% c("Core", "Module")) %>%
            distinct() %>%
            filter(sect_num > 0)
        }, error = function(e) {
          browser()

        })



      },

      # =======================================================================

      # get_layout_X = function() {
      #
      #   ext <- dataset_mgr$get(extent)
      #
      #   df_layout <- self$get_layout_ext(ext)
      #
      #   if(is.null(df_layout)) {
      #     ext <- ifelse(ext == "local", "public", "local")
      #     df_layout <- self$get_layout_ext(ext)
      #   }
      #
      #   if(is.null(df_layout)) {
      #
      #   }
      #
      #
      #   df_layout %>%
      #     mutate(across(where(is.character),
      #                   ~stringi::stri_replace_all_regex(.x, "\\s+", " ")))
      # },

      get_layout_ext = function(extent) {

        version <- dataset_mgr$get(version)
        ext_in <- dataset_mgr$get(extent)

        dataset_mgr$set(extent = extent)

        df_layout <- self$get_merged_layout()

        if(is.null(df_layout) && version == 0) df_layout <- self$get_codebook_layout()

        if(is.null(df_layout)) df_layout <- self$get_sas_layout()

        dataset_mgr$set(extent = ext_in)

        df_layout
      },

      get_codebook_layout = function() {

        file <- private$file_mgr_pvt$apply("codebook_layout_path")

        if(!file.exists(file)) {

          vars <- private$dataset_mgr_pvt$as.list()
          private$dataset_mgr_pvt$set(geog_flag = "off")

          file <- private$file_mgr_pvt$apply("codebook_layout_path")

          if(!file.exists(file)) return(NULL)
        }

        readRDS(file = file)

      },

      get_saq_layout = function(){

        saq_layout <- private$file_mgr_pvt$apply("saq_layout_path")

        readRDS(saq_layout)
      },

      get_merged_layout = function() {

        fldr <- private$file_mgr_pvt$apply("codebook_layout_folder")

        fil <- private$file_mgr_pvt$apply("merged_layout_file")

        file <- paste0(fldr,fil)

        if(!file.exists(file)) return(NULL)


        readRDS(file = file)

      },

      layout_exists = function(type = NULL) {

        file <- self$get_layout_filename(type)

        return(!is.null(file) && file.exists(file))

      },

      get_layout_filename = function(type = NULL) {

        if(is.null(type)) type <- private$type_pvt

        file <- tryCatch({

          fldr <- private$file_mgr_pvt$apply("layout_folder")

          fil <- private$file_mgr_pvt$apply(paste0(type,"_layout_file"))

          paste0(fldr,fil)

        }, error = function(e) {

          return(NULL)

        })

        return(file)

      },

      get_best_type = function() {

        types <- c("merged", "codebook", "sas")

        best <- purrr::map_lgl(types, ~self$layout_exists(.x)) %>%
          which(.) %>% min()

        types[best]
      },

      get_best_layout = function() {

        private$df_layout_pvt <- self$get_layout(self$get_best_type())

      },

      get_layout = function(type = NULL) {

        if(is.null(type)) type <- private$type_pvt

        if(self$layout_exists(type)) {
          file  <- self$get_layout_filename(type)
          df_layout  <- readRDS(file = file)
        } else {
          return(NULL)
        }
        private$prep_layout(df_layout)
      },

      get_sas_layout = function() {

        if(layout_exists("sas")) {
          file  <- get_layout_filename("sas")
          df_layout  <- readRDS(file = file)
        } else {
          return(NULL)
        }
        df_layout
      },



      get_codebook_values = function() {

        fname <- private$file_mgr_pvt$apply("codebook_values_path")

        if(!file.exists(fname)) return(NULL)

        readRDS(file = fname)
      },


      get_merged_values = function() {

        file <-  private$file_mgr_pvt$apply("merged_values_path")

        if(!file.exists(file)) return(NULL)

        readRDS(file = file)

      },

      get_saq_values = function(){

        file <-  private$file_mgr_pvt$apply("saq_values_path")

        if(!file.exists(file)) return(NULL)

        readRDS(file = file)

      }

    ),

    # =======================================================================
    # =======================================================================


    active = list(

      year = function(value) {

        if(missing(value)) return(private$year_pvt)

        if(is.numeric(value)) {
          private$dataset_mgr_pvt$set(year = value)
        }

      },

      layout = function(value) {

        if(!missing(value)) {
          message("This property is read-only")
          return(NULL)
        }

        return(private$df_layout_pvt)
      },

      layout_basic = function(value) {

        if(!missing(value)) {
          message("This property is read-only")
          return(NULL)
        }

        return(
          private$df_layout_pvt %>%
            select(col_name, sect_type, sect_num, section)
        )
      }


    )
  )


#' @export
DataLayout_Mgr <-
  R6Class(
    classname = "DataLayout_Mgr",
    inherit = Layout_Mgr,

    private = list(
      df_layout_data_pvt = NULL,
      df_brfss = NULL
    ),

    public = list(

      initialize = function(df = NULL, ...) {

        super$initialize(...)

        self$load_layout_from_data(df)
      },

      # =======================================================================

      load_layout_from_data = function(df = NULL) {

        if(is.null(private$df_layout_pvt)) {
          private$df_layout_pvt <- super$get_best_layout()
        }

        if(is.null(df)) {

          data_mgr <- DataMgr$new(dataset_mgr = private$dataset_mgr_pvt )
          data_mgr$dataset_mgr$set(year = private$year_pvt)

          #if(is.null(private$df_brfss))
          private$df_brfss <- data_mgr$prepped_data #return(NULL)

          data_yr <- data_mgr$dataset_mgr$get(year)

          df <- private$df_brfss

        }

        df_lo <- purrr::map(df, \(col) {

          attrs <- attributes(col)

          attrs[!names(attrs) %in% c("levels", "class")] %>%
            as.list() %>% as.data.frame() %>%
            mutate(across(ends_with("index"), as.integer)) %>%
            mutate(across(starts_with("is_"), as.logical))

        }) %>%
          bind_rows() %>%
          select(variable, sect_type = section_type, sect_num = section_num,
                 section = section_name, section_index, label, question,
                 any_of(c("is_calculated", "is_custom", "population", "is_saq"))) %>%
          mutate(year = data_yr) %>%
          relocate(year)


        df_lo <- df_lo %>% private$prep_layout()
        private$df_layout_pvt <- df_lo
      }

    ), #  end of public

    active = list(

      year = function(value) {

        if(missing(value)) {

          return(private$year_pvt)

        } else {

          if(is.numeric(value)) {

            private$year_pvt <- value
            private$dataset_mgr_pvt$set(year = value)

            self$load_layout_from_data()
          }
        }
      }
    ) #  end of active
  )
