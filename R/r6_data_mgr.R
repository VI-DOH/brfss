
#' Data Manager Class
#'
#' @description
#' Core manager responsible for handling, searching, extracting, and profiling raw and prepped BRFSS datasets.
#' Coordinates structural layouts and files using internal `DataSetMgr` and `FileMgr` instances.
#'
#' @export
DataMgr <-
  R6::R6Class(
    classname = "DataMgr",

    private = list(

      # ------  properties  -------

      ..layout_mgr = NULL,
      ..dataset_mgr = NULL,
      ..file_mgr = NULL,

      # ------  methods  -------

      ..data_path = function(rw = c("r","w")) {

        p <- private

        read <- rw == 'r'
        write <- rw == 'w'

        params <- p$..dataset_mgr$patterns

        fldr <- p$..file_mgr$apply("annual_data_folder")

        if(!dir.exists(fldr) && write) {
          dir.create(fldr, recursive = TRUE)
        }

        file <- p$..file_mgr$apply("annual_data_file")

        path <- paste0(fldr,file)

        if(read && !file.exists(path)) path <- NULL

        path

      }

    ),

    public = list(

      initialize = function(dataset_mgr = NULL, file_mgr = NULL, ...) {

        if(!is.null(dataset_mgr)) {
          if(inherits(dataset_mgr, "DataSetMgr")) {
            private$..dataset_mgr <-  dataset_mgr
          }
        } else {
          private$..dataset_mgr <-  DataSetMgr$new()
        }

        private$..dataset_mgr$set(...)

        if(!is.null(file_mgr)) {
          if(inherits(file_mgr, "FileMgr")) {
            file_mgr$dataset_mgr$set(...)
            private$..file_mgr <- file_mgr

          }
        } else {
          private$..file_mgr <- FileMgr$new(dataset_mgr = private$..dataset_mgr)

        }


        #layout_mgr_pvt <- Layout_Mgr$new()
      },

      #' @description
      #' Calculate weighted population percentages for specific variables.
      #' @param ... Variables to pass down to the core percentage processing wrapper.
      #' @param include_n (logical) If `TRUE`, appends structural sample sizes (N)
      #' to the table. Default is `TRUE`.
      #' @return A data frame containing weighted percentages and statistical distributions.
      percents = function(..., include_n = TRUE) {

        wt_var <- self$dataset_mgr$get(weight)
        percents(df = self$prepped_data, inc_n = include_n, ...)

      },

      #' @description
      #' Find column/variable names matching a text or regex pattern.
      #' @param pttrn (character) Regex string match filter. Defaults to `""`.
      #' @return A character vector of matching column names.
      col_names = function(pttrn = "") {

        self$col_info() %>%
          filter(grepl(pttrn, variable)) %>%
          pull(variable)



      },

      #' @description
      #' Extract detailed structural dictionary metadata and labels embedded within columns.
      #' @param section (character) Filter for section names. Defaults to regex `".*"`.
      #' @param label (character) Filter for attribute label descriptors.
      #' Defaults to regex `".*"`.
      #' @return A data frame containing variable definitions, section tags,
      #' and descriptive labels.
      col_info = function(section = ".*", label = ".*") {

        df <- self$prepped_data

        ncols <- ncol(df)

        df_atts <- purrr::map(1:ncols, \(icol) {

          df0 <-
            tryCatch(expr = {
              attrs <- attributes(df[[icol]])

              if("variable" %in% names(attrs)) {

                variable      <- attrs[["variable"]] %||% NA
                section_type  <- attrs[["section_type"]] %||% NA
                section_num   <- attrs[["section_num"]] %||% NA %>% as.character()
                section_index <- attrs[["section_index"]] %||% NA %>% as.character()
                section_name  <- attrs[["section_name"]] %||% NA
                label         <- attrs[["label"]] %||% NA

                data.frame(variable, section_type,
                           section_num, section_index,
                           section_name, label)
              } else {

                data.frame()
              }

            }, error = function(e) {
              #browser()
            }
            )

          df0
        }) %>%
          bind_rows()

        df_atts %>%
          filter(grepl(section, section_name)) %>%
          filter(grepl(.env$label, label))

      },

      #' @description
      #' Export the active internal data frame out to a standard tabular CSV text file.
      #' @param filename (character) Complete string path location where the CSV file
      #' should be written.
      to_csv = function(filename) {

        if(missing(filename)) {
          message("You must specify a filename")
          return()
        }
        df <- self$data()
        df %>% write.csv(filename)

      },

      #' @description
      #' Profile the local workspace file system for existing RDS cache files.
      #' @return A tracked summary data frame sorted chronologically by year, geography,
      #' and code layout variants.
      my_local_data = function() {

        p <- private

        params <- p$..dataset_mgr$as.list()

        dir <- p$..file_mgr$apply("data_folder")

        df <- list.files(dir, pattern = "^.._20[0-9]{2}.rds$", recursive = TRUE) %>%
          grep("^[0-9]{4}/local/", ., value = TRUE) %>%
          grep("../(sas|ascii)",., value = TRUE) %>%
          as.data.frame() %>%
          rename(filename = 1) %>%
          mutate(base = basename(filename)) %>%
          mutate(geog = gsub("(..)_([0-9]{4}).*", "\\1", base))%>%
          mutate(year = gsub("(..)_([0-9]{4}).*", "\\2", base))%>%
          mutate(source = gsub(".*(ascii|sas).*", "\\1", filename)) %>%
          relocate(filename, .after = last_col())

        df_ascii <- df %>% filter(source == "ascii")
        df_sas <- df %>% filter(source == "sas")

        df_sas_only <- df_sas %>% anti_join(df_ascii %>% select(year), by = join_by(year))
        df_ascii_only <- df_ascii %>% anti_join(df_sas %>% select(year), by = join_by(year))

        df_ascii %>%
          bind_rows(df_sas_only) %>%
          arrange(year)
      },



      #' @description
      #' Scan local data structures to locate public state data frames that have been cataloged.
      #' @param year (integer) Optional explicit year restriction filter.
      #' @return A structural dataframe highlighting years, geographies, and storage types.
      my_public_geogs = function(year = NULL) {

        p <- private

        params <- p$..dataset_mgr$as.list()

        dir <- p$..file_mgr$apply("data_folder")

        df <-  list.files(dir, recursive = TRUE) %>%
          grep("^[0-9]{4}/public/states", ., value = TRUE) %>%
          grep(".*states/../(sas|ascii)",., value = TRUE) %>%
          as.data.frame() %>%
          rename(filename = 1) %>%
          mutate(test = gsub("/public/states", "", filename)) %>%
          mutate(year = gsub("([0-9]{4}).*", "\\1", test)) %>%
          mutate(geog = gsub("([0-9]{4})/(.*?)/.*", "\\2", test)) %>%
          mutate(source = gsub("([0-9]{4})/(.*?)/(.*?)/.*", "\\3", test)) %>%
          mutate(file = gsub("([0-9]{4})/(.*?)/(.*?)/(.*)", "\\4", test)) %>%
          mutate(version = gsub(".*[0-9]{4}(.*)[.]rds", "\\1", file)) %>%
          mutate(version = as.integer(gsub("_V", "", version))) %>%
          select(-filename, -test) %>%
          relocate(version, .before = file)

        if(!is.null(year)) {
          df <- df %>% filter(year == .env$year)
        }

        df
      },

      #' @description
      #' Read the structured BRFSS dataset from disk with custom attributes appended.
      #' @return An object of class `brfss_data` and `data.frame` complete with configuration tracking keys.
      data = function() {

        fname <- private$..data_path(rw = 'r')

        if(is.null(fname))  return(NULL)

        df_brfss<- readRDS(fname)

        params <- private$..dataset_mgr$patterns

        structure(df_brfss,
                  class = c("brfss_data", "data.frame"),
                  year = params["YEAR"] %>% as.integer() %>% unname(),
                  geog = params["GEOG"] %>% unname(),
                  source = params["SRC"] %>% unname(),
                  extent = params["EXT"] %>% unname(),
                  version = params["VERS"] %>% unname()
        )
      },

      #' @description
      #' Pass updates directly down to the internal dataset configurations framework.
      #' @param ... Named attribute-value pairs to set.
      set = function(...) {

        private$..dataset_mgr$set(...)
      },

      #' @description
      #' Retrieve parameters directly out of the internal dataset configuration framework.
      #' @param ... Named variable values to request.
      get = function(...) {

        private$..dataset_mgr$get(...)
      }


    ),

    active = list(

      #' @field modules Read-only list containing the current year's BRFSS module matrix definitions.
      modules = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        params <- private$..dataset_mgr$patterns

        file <- private$..file_mgr$apply("brfss_modules_path")

        readRDS(file)

      },

      #' @field has_data Read-only indicator checking if an annual dataset matches the path profile.
      has_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        p <- private

        file <- p$..file_mgr$apply("annual_data_path")

        file.exists(file)

      },

      #' @field has_raw_data Read-only indicator checking for raw localized dataset files on disk.
      has_raw_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        p <- private

        file <- p$..file_mgr$apply("annual_data_path")

        file.exists(file)

      },

      #' @field prepped_data Read-only binding that executes data cleaning routines and transforms inputs into factors.
      prepped_data = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        params <-  private$..dataset_mgr$as.list()

        func_any <- paste0("prepped_", params$year)
        func_geog <- paste0("prepped_", params$year, "_", params$geog)

        df <- self$data()

        if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>%
          which()

        if(is.null(df) || length(f) == 0) {

          source <- private$..dataset_mgr$get(source)

          try_source <- ifelse (source == "ascii", "sas", "ascii") %>% unname()

          private$..dataset_mgr$set(source = try_source)

          df <- self$data()

          private$..dataset_mgr$set(source = source)
        }

        if(is.null(df))  return(NULL)

        attribs <-  attributes(df)
        attribs <-  attribs[!names(attribs) %in% c("row.names", "names",
                                                   "class")]
        if(func_any %in% ls(.GlobalEnv)) {
          df <- do.call(func_any, args = list(df))
        }

        if(func_geog %in% ls(.GlobalEnv)) {

          df <- do.call(func_geog,args = list(df))
        }

        do.call(structure,
                args = c(list(df,
                              class = c("brfss_prepped", "data.frame")), attribs))
      },

      #' @field params Read-only snapshot list of parameters tracked inside the inner configuration state.
      params = function(value) {

        if(!missing(value)) {
          message("this property is read-only")
          return(NULL)
        }

        private$..dataset_mgr$as.list()

      },

      #' @field year Accessor and mutator string key referencing the target study cycle year.
      year = function(value) {

        if(missing(value)) {
          return(self$dataset_mgr$get(year))
        } else {
          if(is.numeric(value)) {
            self$dataset_mgr$set(year = value)
          }
        }
      },

      #' @field dataset_mgr Active instance of the dataset parameter configuration manager.
      dataset_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$..dataset_mgr <-  value
            private$..file_mgr <- FileMgr$new(dataset_mgr = value)
          }
        }
        private$..dataset_mgr

      },


      #' @field file_mgr Active instance of the file path conversion manager.
      file_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "FileMgr")) {
            private$..file_mgr <- FileMgr$new(dataset_mgr = value)
          }
        }
        private$..file_mgr

      }


    )
  )



#' Public Data Manager Class
#'
#' @description
#' Extended manager designed for processing public-release BRFSS components, handling state subsets,
#' and isolating state-level sample sizes.
#'
#' @seealso Inherits directly from \code{\code{\link{DataMgr}}}.
#' @export
PublicDataMgr <-
  R6::R6Class(
    classname = "PublicDataMgr",
    inherit = DataMgr,

    private = list(
    ),

    public = list(

      initialize = function(...) {

        super$initialize(extent = "public", ...)

      },

      #' @description
      #' Cross-tabulate record counts across available survey versions for a given state tracking marker.
      #' @return A grouped lookup data frame containing regional metrics and records counts.
      state_counts = function() {

        dataset_mgr <- PublicDataSetMgr$new()

        dataset_mgr$set(year = 2024)
        dataset_mgr$set(geog_flag = "off")


        data_mgr <- DataMgr$new(dataset_mgr = dataset_mgr)

        df_records <- purrr::map(0:3, \(version) {

          dataset_mgr$set(version = version)


          data_mgr$data %>% summarise(n = n(), .by = c(`_STATE`)) %>%
            mutate(version = .env$version)


        }) %>%
          bind_rows()

        df_records %>%
          left_join(df_geogs, by = join_by(`_STATE` == Id)) %>%
          select(geog = Abbrev, version, n)

        df_records



      },

      #' @description
      #' Retrieve public survey tables, handling automatic falling back filtering based on state FIPS boundaries.
      #' @param any (logical) If `TRUE`, falls back to checking state-wide scopes if localized file definitions are missing. Default is `TRUE`.
      #' @return A data frame containing public survey entries.
      data = function(any = TRUE) {

        df <- super$data()

        if(is.null(df) && any) {

          geog_flag <- super$dataset_mgr$get(geog_flag)

          if(geog_flag == "on") {

            geog <- super$dataset_mgr$get(geog)
            gm <- GeogMgr$new(geog = geog)
            fips <- gm$geog %>% pull(fips)
            super$dataset_mgr$set(geog_flag = "off")
            df <- super$data() %>% filter(`_STATE` == fips)
            super$dataset_mgr$set(geog_flag = "on")

          }
        }

        df
      },

      #' @description
      #' Extract, isolate, and save a specific geographical slice out to a dedicated localized file.
      #' @param geog (character) Geographic identifier (e.g., FIPS, abbreviation, or name string).
      save = function(geog = NULL) {

        if(!is.null(geog)) {

          dataset_mgr <- super$dataset_mgr
          file_mgr <- FileMgr$new(dataset_mgr = dataset_mgr)

          geog_flag <- dataset_mgr$get(geog_flag)

          dataset_mgr$set(geog_flag = "off")
          gm <- GeogMgr$new(geog = geog)
          fips <- gm$geog %>% pull(fips)

          if(!is.null(fips)) {
            dataset_mgr$set(geog_flag = "off")
            df <- super$data() %>% filter(`_STATE` == fips)

            dataset_mgr$set(geog = geog)

            file <- file_mgr$apply("annual_data_path")
            df %>% saveRDS(file)
            dataset_mgr$set(geog_flag = geog_flag)
          }

        }
      }

    ),

    active = list(

      #' @field geog Active binding to get or alter the regional targeting configuration parameter.
      geog = function(value) {

        if(missing(value)) return(super$dataset_mgr$get(geog))

        super$dataset_mgr$set(geog = value)
      }
    )
  )

#' Local Data Manager Class
#'
#' @description
#' Extended manager subclass specialized in handling local or regional-level subsets of BRFSS data.
#' Automatically configures and targets data structures with an extent set to "local".
#'
#' @seealso Inherits directly from \code{\link{DataMgr}}.
#' @export
LocalDataMgr <-
  R6::R6Class(
    classname = "LocalDataMgr",
    inherit = DataMgr,

    private = list(
    ),

    public = list(

      initialize = function(...) {

        super$initialize(extent = "local", ...)

      }
    ),

    active = list(

    )
  )



#' National Data Manager Class
#'
#' @description
#' Extended manager subclass focused on aggregating, processing, and rendering national-level comparisons
#' across multiple states, divisions, or regions. Interoperates with Codebook and Raw Data managers.
#'
#' @seealso Inherits directly from \code{\link{DataMgr}}.
#' @export
NationalDataMgr <-
  R6::R6Class(
    classname = "NationalDataMgr",
    inherit = DataMgr,

    private = list(
      ..natl_dataset_mgr = NULL,
      ..group = "each",
      ..col = NULL,
      ..roi = "^Yes$",
      ..my_geog = "",
      ..data = NULL ,

      # --------------------------------------------------------------------------------------------
      #
      #   build the data frame
      #
      ..calc_natl_pcts = function() {

        p <- private

        col <- p$..col
        roi <- p$..roi

        if(is.null(col)) return(NULL)

        dataset_mgr <- self$dataset_mgr

        cb_mgr <- CodebookMgr$new(dataset_mgr = dataset_mgr)

        df_lo <- cb_mgr$get_layout() %>%
          filter(col_name == col)

        bad_col <- df_lo %>%
          {nrow(.) != 1}

        if(bad_col) {
          message(paste0("col (", col, ") does not exist"))
          return(NULL)
        }


        df <- self$get_one_column(col = col)

        if(is.null(df)) {

          message("processed ",dataset_mgr$get(extent), " " ,
                  dataset_mgr$get(source), " data not available for ",
                  dataset_mgr$get(year), "\n",
                  "... trying raw data")

          if(dataset_mgr$get(source) == "ascii") {
            rd_mgr <- AsciiRawDataMgr$new(dataset_mgr = dataset_mgr)
          } else {
            rd_mgr <- SasRawDataMgr$new(dataset_mgr = dataset_mgr)
          }

          df <- rd_mgr$get_one_column(col = col)

        }

        df_each <- df %>%
          group_by(`_STATE`, .data[[col]]) %>%
          summarise(n = sum(`_LLCPWT`), .groups = "drop")

        df_each_den <- df_each %>%
          group_by(`_STATE`) %>%
          summarise(den = sum(n), .groups = "drop")

        df <- df_each %>%
          left_join(df_each_den, by = join_by(`_STATE`)) %>%
          mutate(pct = round(n/den*100,1))  %>%
          filter(grepl(roi, .data[[col]]))

        response <- df %>% pull(.data[[col]]) %>% as.character() %>% unique()

        df <- df %>%
          left_join(GeogMgr$geogs(), by = join_by(`_STATE` == fips)) %>%
          select(name, abbr, region, division, pct) %>%
          as.data.frame()

        df <- df %>%
          ungroup() %>%
          arrange(desc(pct))

        p$..data <-
          structure(df,
                    year = dataset_mgr$get(year),
                    col_name = df_lo$col_name,
                    label = df_lo$label,
                    response = response,
                    by = by)
      },

      ..match_group = function(grp) {

        match.arg(grp, c("each", "region", "division"))

      },

      # -----------------------------------------------------------------------------
      ..national_plot_each = function(my_geog = NULL, group = NULL, agg = FALSE,
                                      clrs = NULL,
                                      title = NULL,
                                      subtitle = NULL) {

        p <- private

        if(!agg)
          clrs <- clrs %||% RColorBrewer::brewer.pal(n=12, name = "Set3")

        else
          clrs <-   clrs %||% c("dodgerblue",  "darkgreen")



        if(is.null(group)) {
          group <- p$..group

        } else {

          group <- p$..match_group(group)
        }

        #df_natl <- self$national(group)

        df_natl <- self$table

        attrs <- df_natl %>% attributes()

        title <- title %||% attrs$label
        subtitle <- subtitle %||% attrs$year
        caption <- title
        by <- attrs$by


        if(group != "each") return(NULL)

        df_natl <- df_natl %>%
          mutate(me = (abbr == my_geog))

        if(agg && my_geog != "") {

          nm <- GeogMgr$full_name(my_geog)

          df_gpl <- df_natl %>%
            group_by(me) %>%
            summarise(pct = median(pct), .groups = "drop") %>%
            mutate(name = if_else(me, nm, "Other"))

        } else {
          lvls <- df_natl %>% pull(name)

          df_gpl <- df_natl %>%
            mutate(name = factor(name, levels = lvls)) %>%
            mutate(me = abbr == {{my_geog}})
        }

        labels<-sprintf("%0.1f",df_gpl[["pct"]])

        if(is.null(my_geog) || my_geog == "") {
          fill <- "region"
          legend_pos <- "right"
        } else {
          fill <- "me"
          legend_pos <- "none"
        }

        gpl<- df_gpl %>%

          ggplot2::ggplot(ggplot2::aes(x = name, y=pct, fill = .data[[fill]])) +
          ggplot2::geom_bar(stat = "identity") +

          ggplot2::geom_text(size=3,label=labels,
                             show.legend=F, hjust=-0.1, vjust=0.5) + #,
          # position=position_dodge(width=dodge_width), vjust=vjust) +

          # scale_fill_gradient(limits=c(fmin,fmax), low = "#13FF00", high = "#FF1311", space = "Lab",
          #                     na.value = "grey50", guide = "colourbar") +
          ggplot2::scale_fill_manual(values = clrs) +

          ggplot2::coord_flip() +
          #      geom_errorbar(aes_string(ymin="CI_lower",ymax="CI_upper"),
          #                    width=error_bar_width,color="blue") +
          ggplot2::labs(title=title, subtitle=subtitle, caption = caption) +

          ggplot2::xlab("") +
          #  scale_y_continuous(limits = c(0,national_rate_max)) +
          #      scale_x_discrete(labels=labels) +
          ggplot2::theme(
            panel.grid.major.x =
              ggplot2::element_line(colour = "#999999", linewidth = 0,
                                    linetype=1, lineend="butt"),
            panel.background = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position = legend_pos)

        gpl

      },

      # -------------------------------------------------------------------------------------------
      ..national_plot_sub = function(my_geog = "", agg = FALSE, group = NULL,
                                     clrs = NULL,
                                     title = NULL,
                                     subtitle = NULL) {

        p <- private


        if(is.null(group)) {
          group <- p$..group

        } else {

          group <- p$..match_group(group)
        }

        clrs <- clrs %||% RColorBrewer::brewer.pal(n=60, name = "Set3")

        df_natl <- self$national(group = group)

        attrs <- df_natl %>% attributes()

        title <- title %||% attrs$label
        subtitle <- subtitle %||% attrs$year
        caption <- title
        by <- attrs$by

        if(group == "self") return(NULL)

        df_natl <- df_natl %>%
          rename_with(.fn = ~gsub("division|region", "name", .x)) %>%
          mutate(me = grepl(my_geog, name))

        if(agg && my_geog != "") {

          df_gpl <- df_natl %>%
            group_by(me) %>%
            summarise(pct = median(pct), .groups = "drop") %>%
            mutate(name = if_else(me, name, "Other"))

        } else {
          lvls <- df_natl %>% pull(name)

          df_gpl <- df_natl %>%
            mutate(name = factor(name, levels = lvls))
        }

        labels<-sprintf("%0.1f",df_gpl[["pct"]])


        gpl<- df_gpl %>%

          ggplot2::ggplot(ggplot2::aes(x = name, y = pct, fill = name)) +
          ggplot2::geom_bar(stat = "identity") +

          ggplot2::geom_text(size=3,label=labels,
                             show.legend=F, hjust=-0.1, vjust=0.5) + #,

          ggplot2::scale_fill_manual(values = clrs) +

          ggplot2::coord_flip() +

          ggplot2::labs(title=title, subtitle=subtitle, caption = caption) +

          ggplot2::xlab(group) +

          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(colour = "#999999", linewidth = 0, linetype=1,
                                                       lineend="butt"),
            panel.background = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position = "none")

        gpl

      }
    ),

    public = list(

      #' @description
      #' Initialize a new National Data Manager instance.
      #' @param col (character) The tracking target dataset column name to isolate and
      #' calculate.
      #' @param roi (character) Regular expression defining the "Response of Interest"
      #' (e.g., `"^Yes$"`).
      #' @param group (character) Categorical unit aggregation grouping variable level
      #' (`"each"`, `"region"`, or `"division"`).
      #' @param ... Additional arguments forwarded to the parent `DataMgr` initialization step.
      #'
      initialize = function(col = NULL, roi = NULL, group = NULL, ...) {

        p <- private

        super$initialize(extent = "public", ...)
        p$..natl_dataset_mgr <- super$dataset_mgr$clone(deep = TRUE)

        p$..roi <- p$..roi %||% roi
        self$group <- p$..group %||% group

        self$column <- col


      },

      geog_data = function(geog = NULL) {

        p <- private

        geog <- geog %||% p$..natl_dataset_mgr$get(geog)
        if(is.null(geog)) return(NULL)

        fips <- GeogMgr$as_fips(geog)
        p$..data %>% filter(`_STATE` == fips)


      },

      #' @description
      #' Retrieve, aggregate, and calculate percents for a column with option to add
      #' a prep function first.  This allows analysis for custom variables.
      #' @param col (character) Column variable name target to calculate.
      #' @param col_raw (character) Optional regular expression mapping target string
      #' answers of interest.
      #' @param prep (function) Optional function to call to prep the <col> from the <raw_col>.
      #' @return A data frame containing percents for all responses for the column of interest.
      #'
      wide = function(col, prep, raw_col) {

        if(missing(col)) {

          message("Must provide <col>")
          return(NULL)
        }

        if(missing(raw_col)) {
          raw_col <- col
        } else {
          if(missing(prep)) {

            message("Must provide a prep function if <raw_col> is supplied")
            return(NULL)

          }
        }

        gm <- GeogMgr$new()

        df_geogs <- GeogMgr$new()$geogs %>% select(fips, abbr) %>%
          mutate(fips = as.numeric(fips))

        df <- self$data() %>%
          select(`_STATE`, `_LLCPWT`, all_of(raw_col)) %>%
          filter(!is.na({{raw_col}})) %>%
          rename(Geog = `_STATE`)

        if(!is.factor(df[[raw_col]])) {

          cb_mgr <- CodebookMgr$new()
          df_lo <- cb_mgr$get_layout() %>% filter(col_name == raw_col)
          df_vals <- cb_mgr$get_values() %>% filter(col_name == raw_col)

          df <- brfss::make_factors(df, df_layout = df_lo, df_vals = df_vals, cols = raw_col)
        }
        if(missing(prep)) {
          df2 <- df
        } else {
          df2 <- do.call(prep, list(df, raw_col))
        }

        df_totals <- df2 %>%
          filter(!is.na(.data[[col]])) %>%
          group_by(Geog, .data[[col]]) %>%
          summarise(n = sum(`_LLCPWT`), .groups = "drop") %>%
          left_join(df_geogs,  join_by(Geog == fips))  %>%
          select(-Geog) %>%
          relocate(Geog = abbr) %>%
          tidyr::pivot_wider(names_from = {{col}}, values_from = n)  %>%
          rowwise() %>%
          mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
          ungroup()

        resps <- setdiff(colnames(df_totals), c("Total", "Geog"))

        df_pcts <- df_totals %>%
          mutate(across(.cols = all_of(resps),
                        .fns = ~round(.x/Total * 100,1))
          ) %>%
          select(-Total) %>%
          rowwise() %>%
          mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
          ungroup()

        df_pcts
      },

      #' @description
      #' Retrieve, aggregate, and calculate median percentage frequencies based on
      #' an active grouping state.
      #' @param col (character) Column variable name target to calculate.
      #' @param roi (character) Optional regular expression mapping target string
      #' answers of interest.
      #' @param group (character) Optional metric grouping level override.
      #' @return A sorted reference data frame containing contextual survey attributes.
      national = function(col, roi = NULL, group = NULL) {

        p <- private

        if(is.null(group)) {
          group <- p$..group

        } else {

          group <- p$..match_group(group)
        }

        df <- p$..data

        attrs <- df %>% attributes()


        cb_mgr <- CodebookMgr$new(dataset_mgr = dataset_mgr)

        df_lo <- cb_mgr$get_layout() %>%
          filter(col_name == p$..col)

        year <- super$dataset_mgr$get(year)

        grp <- group

        if(group == "each") {
          grp <- c("name", "abbr")
        }

        df <- df %>%
          group_by(pick(all_of(grp))) %>%
          summarise(pct = median(pct), .groups = "drop")

        df %>%
          arrange(desc(pct)) %>%
          structure(
            year =attrs$year,
            col_name = attrs$col_name,
            label = attrs$label,
            response = attrs$response,
            by = attrs$by)


      },

      #' @description
      #' Extract data records for a single targeted column, automatically
      #' merging primary state and sampling weight attributes.
      #' @param col (character) The exact variable name identifier inside the dataset.
      #' @return A filtered data frame containing state, weight, and target fields,
      #' or `NULL` if missing.
      get_one_column = function(col) {


        df <- super$data()

        if(is.null(df) || (length(df) == 1 && is.na(df)) || nrow(df) == 0) return(NULL)

        if(!col %in% colnames(df)) {

          return(NULL)
        }

        df <- df %>%
          select(`_STATE`, `_LLCPWT`, all_of(col))

        if(private$dataset_mgr$get(geog_flag) == "on") {

          df <- df %>% filter(`_STATE` == private$dataset_mgr$get(geog))
        }

        df
      },

      #' @description
      #' Render an hierarchical geographic tree-map displaying regional frequency
      #' breakdowns via `treemapify`.
      #' @param df_natl An optional pre-calculated data frame from the `$national()`
      #' pipeline. If missing, it executes automatically.
      #' @param ... Additional layout parameters forwarded downstream to control
      #' mapping aesthetics.
      #' @return A `ggplot` object configured for tree-map display elements.
      treemap = function(df_natl, ...) {

        if(missing(df_natl)) df_natl <- national(...)

        ggplot(df_natl, aes(
          area = pct,
          fill = division,          # Parent rectangles get a unified color base
          subgroup = division,      # Defines the primary outer boundary
          subgroup2 = name,      # Defines the primary outer boundary
          label = paste0(name , "\n", pct)      # Labels the inner secondary boxes
        )) +
          geom_treemap() +
          # Adds thick, distinct borders around the parent categories
          geom_treemap_subgroup_border(colour = "white", size = 4) +
          # Adds thick, distinct borders around the parent categories
          geom_treemap_subgroup2_border(colour = "#ddddaa", size = 1) +
          # Adds text labels inside the secondary inner rectangles
          geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
          # Adds a massive heading label for the parent groups
          geom_treemap_subgroup_text(
            place = "topleft",
            colour = "black",
            alpha = 0.6,
            size = 24,
            fontface = "italic"
          )
      },


      #' @description
      #' Primary visualization steering method that selects and calls internal
      #' plotting systems based on grouping specifications.
      #' @param my_geog (character) Optional geographic state tracking shortcut
      #' identifier.
      #' @param group (character) Optional metric layout grouping override constraint.
      #' @param agg (logical) Whether or not median parameters should aggregate across
      #' remaining records. Default is `FALSE`.
      #' @param title (character) Optional descriptive heading layout title.
      #' @param subtitle (character) Optional secondary subtitle layout marker text.
      #' @param ... Parameters passed directly into downstream sub-plot execution tracks.
      #' @return A structural `ggplot` rendering ready for viewing.
      national_plot = function(my_geog = NULL, group = NULL, agg = FALSE,
                               title = NULL,
                               subtitle = NULL, ...) {

        p <- private
        if(is.null(group)) {
          group <- p$..group
        } else {

          group <- p$..match_group(group)
        }

        my_geog <- my_geog %||% p$..my_geog

        if(group == "each")
          p$..national_plot_each(my_geog = my_geog, agg = agg,
                                 title = title,
                                 subtitle = subtitle, ...)
        else
          p$..national_plot_sub(my_geog = my_geog, agg = agg, group = group,
                                title = title,
                                subtitle = subtitle, ...)
      }


    ),

    active = list(

      #' @field table Read-only accessor property exposing the underlying computed data table context structure.
      table = function(value) {

        if(!missing(value)) {
          message("This property is read-only")
          return(NULL)
        }

        private$..data
      },

      #' @field group Accessor and modifier binding managing geographic pooling frameworks (`"each"`, `"region"`, or `"division"`).
      group = function(value) {

        if(!missing(value)) {
          grp <- private$..match_group(value)

          private$..group <- grp

          return()
        }

        private$..group
      },

      #' @field column Accessor and modifier tracking variable that automatically triggers national percentage calculation algorithms when altered.
      column = function(value) {

        p <- private

        if(!missing(value)) {

          private$..col <- value

          if(!is.null(value)) {

            p$..calc_natl_pcts()

          }
          return()
        }

        p$..col
      }
    )
  )


