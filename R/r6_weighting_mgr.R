#' @title WeightingMgr
#' @description A class for aworking with survey weights
#' @family BRFSS WeightingMgr
#' #' @export
WeightingMgr <- R6::R6Class(

  classname = "WeightingMgr",

  private = list(

    ..data_mgr = NULL,
    ..dataset_mgr = NULL,

    ..df_brfss = NULL
  ),

  public = list(

    initialize = function(dataset_mgr = NULL) {

      if(is.null(dataset_mgr)) {
        dataset_mgr <- DataSetMgr$new()
      }

      private$..data_mgr <- DataMgr$new(dataset_mgr = dataset_mgr)

      private$..dataset_mgr <- dataset_mgr
     # private$..df_brfss <- private$..data_mgr$data()

    },

    get_margin = function(margin = 1) {

      dsm <- private$..dataset_mgr

      extent <- dsm$get("extent")
      if(dsm$get("geog_flag") == "off" || extent == "public") {
        cli::cli_abort(c("Margins are only available for local datasets",
                         "i" = "This objects DatasetMgr object has extent set to {extent}")
        )
      }

      col <- paste0("_LLCPM", sprintf("%02d", margin))
      df_brfss <- private$..data_mgr$data()

      df_brfss %>% pull(matches(col))

    },

    data = function() {
      private$..data_mgr$data()
    },

    weight_distrib = function(df = NULL,
                              type = c("histogram", "boxplot",  "summary"),
                              bin_width = 5, mult = NULL) {

      type <- match.arg(type,
                        choices = c("histogram", "boxplot",  "summary") )

      if(is.null(df)) df <- self$data()

      if(is.null(df) || nrow(df) == 0) return(NULL)

      params <- private$..dataset_mgr$as.vector()

      total <- sum(df$`_LLCPWT`)

      str_total <- total %>% format(big.mark = ",", scientific = FALSE)


      if(type == "boxplot") {

        x <- weight_distrib(2024, type = "summary")

        df %>% ggplot(aes(y = `_LLCPWT`)) +
          geom_boxplot(fill = "steelblue", alpha = 0.6) +
          labs(title = "Distribution of `_LLCPWT`") +
          theme_minimal()

      } else  if(type == "histogram") {

        df %>% ggplot( aes(x = `_LLCPWT`)) +
          geom_histogram(binwidth = bin_width, fill = "steelblue", alpha = 0.6) +
          geom_vline(aes(xintercept = median(`_LLCPWT`, na.rm = TRUE)),
                     color = "red", linetype = "dashed", linewidth = 1) +
          geom_vline(aes(xintercept = mean(`_LLCPWT`, na.rm = TRUE)),
                     color = "darkgreen", linetype = "dotted", linewidth = 1) +
          labs(title = "Distribution of `_LLCPWT` with Mean & Median",
               subtitle = paste0(params["geog"], " - ", params["year"], "\n",
                                 "Adult Pop: ", str_total)) +
          xlab("Final Weight") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 15),
            plot.subtitle = element_text(size = 14)
          )

      } else  if(type == "summary") {

        sum <- df %>% pull(`_LLCPWT`) %>% summary()

        if(!is.null(mult)) {
          sum <- sum / total * mult
        } else {
          return(sum %>% c(Total = total))
        }

      }

    },

    natl_weights = function(year, state ) {

      p <- private

      dm <- p$..data_mgr$clone(deep = TRUE)
      dsm <- dm$dataset_mgr$clone(deep=TRUE)

      if(!missing(state)) {
        dsm$set(geog = state)
        dsm$set(geog_flag = "on")
      }

      if(!missing(year)) dsm$set(year = year)

      params <- dsm$as.list()

      gflag <- dsm$get(geog_flag) == "on"
      geog <- dsm$get(geog)

      geog_txt <- if(gflag) {
        paste0(" for ", geog)
      } else {
        ""
      }


      has_data <- dm$has_data

      browser()
      if(!has_data) {

        has_data <- dm$has_raw_data
        raw <- FALSE

      } else {
        raw <- TRUE
      }


      if(!has_data) {

        msg <- paste0("==========================================================================\n",
                      stringr::str_to_title(params$extent), " ", params$source, " data ",geog_txt, " for the year ",
                      params$year, " are not available")

        message(msg)
        return(NULL)
      } else {

        type_txt <- if(raw) {
          "Raw "
        } else {
          "Processed "
        }
        msg <- paste0("==========================================================================\n",
                      type_txt, params$extent, " ", params$source, " data",geog_txt, " for the year ",
                      params$year, " have been located")

        message(msg)
        return(NULL)
      }


      browser()
      file_mgr$get("ascii_raw_data_folder")
      asc_file <- file_mgr$apply("ascii_path_raw")


      cb_mgr <- brfss::CodebookMgr$new(dataset_mgr = ds_mgr)

      df_lo <- cb_mgr$get_layout() %>%
        dplyr::filter(col_name %in% c("_STATE", "_LLCPWT"))


      col_names  <-  df_lo %>% pull(col_name)

      cols <- readr::fwf_positions(
        start = df_lo %>% pull(start),
        end   = df_lo %>% pull(end),
        col_names = df_lo %>% pull(col_name)
      )

      col_types <- df_lo %>% pull(var_type) %>% tolower() %>% substring(1,1)


      suppressMessages(
        df <- readr::read_fwf(file = asc_file, col_positions = cols,
                              readr::cols(.default = "c") ) %>%
          readr::type_convert()
      )

      if(!missing(state)) {

        fips <- GeogMgr$new(geog = state)$fips
        df <- df %>% filter(`_STATE` == fips)
      }

      total <- as.integer(sum(df$`_LLCPWT`))
      sd <- sd(df$`_LLCPWT`)

      options(scipen = 999)

      smry <- summary(df)

      x <- smry %>%
        as.data.frame() %>% filter(grepl("_LLCPWT",Var2)) %>%
        mutate(state = state) %>%
        mutate(year = year) %>%
        mutate(item = stringr::str_trim(gsub("(.*)[:].*", "\\1", Freq))) %>%
        mutate(value = as.numeric(
          stringr::str_trim(gsub("(.*)[:](.*)", "\\2", Freq)))
        ) %>%
        select(-starts_with("Var"), -Freq) %>%
        mutate(per10K = round(value/total * 10000,1)) %>%
        mutate(value = round(value, 1)) %>%
        tibble::add_row(year = year, state = state, item = "SD", value = sd)

      mean <- x %>% filter(item == "Mean") %>% pull(value)
      median <- x %>% filter(item == "Median") %>% pull(value)

      skew <- 3 * (mean - median)/sd

      x <- x %>%
        tibble::add_row(year = year, state = state, item = "Skew", value = skew)

      structure(x,
                class = c("weight_sum", "data.frame"),
                total = total %>% as.integer(),
                year = year,
                geog = state,
                sd = sd
      )
    },



    all_natl_weights = function(per_100k = FALSE) {

      df <- self$data() |> mutate(fips = as.integer(`_STATE`)) |>
        select(fips, `_LLCPWT`)

      df_tots <- purrr::pmap(GeogMgr$geogs(), \(...) {
        current_row <- list(...)

        fips <- current_row$fips |> as.integer()

        abbr <- GeogMgr$as_abbrev(fips)

        df1 <- df |> filter(fips == {{fips}})

        pop <- sum(df1$`_LLCPWT`)
        intvs <- nrow(df1)

        summary(df1$`_LLCPWT`) |> as.list() |> as.data.frame() |>
          mutate(geog = abbr) |>
          mutate(pop = {{pop}}) |>
          mutate(intvws = {{intvs}})
      }) |>
        bind_rows() |>
        relocate(geog)

      if(per_100k) {

        pct_cols <- setdiff(colnames(df_tots), c("geog", "pop", "intvws"))

        df_tots <- df_tots |>
          rowwise() |>
          mutate(across(.cols = all_of(pct_cols), .fns = ~.x/pop*100000))
      }
      df_tots |> as.data.frame()

    },

    national_weight_plot = function(df = NULL, year = NULL,
                                    my_state = NULL, stat = "Max") {

      if(is.null(df) ) {

        df <- self$all_natl_weights(year)
      }

      year <- df %>% attr("year")

      df <- df %>%
        tidyr::pivot_wider(names_from = item, values_from = per10K,
                           id_cols = state) %>%
        as.data.frame() %>%
        rename_with(.fn = ~gsub("[. ]", "", .x))

      me <- my_state

      clrs <- c("#ccf", "#55cc99")

      names(clrs) <- c("Other", me)

      df_plot <- df_2024 %>%
        mutate(clr = factor(if_else(state == me, me, "Other"))) %>%
        arrange(.data[[stat]]) %>%
        filter(!is.na(Min)) %>%
        mutate(state = factor(state, levels = .data$state))

      df_plot %>%
        ggplot(aes(x = state, y = .data[[stat]], fill = clr)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        coord_flip() +
        scale_fill_manual(values = clrs) +
        ggtitle(paste0("Weighting Comparison\n", year,
                       "\n", stat, " Respondent Weight per 100K population"))

    }

  ),

  active = list(

    dataset_mgr = function(value) {
      if(!missing(value)) {
        if(inherits(value, "DataSetMgr")) {
          private$..dataset_mgr <- value
        }
      }

      return(private$..dataset_mgr)

    }
  )

)


print.weight_sum <- function(x) {

  year <- attr(x, "year")
  geog <- state.name[state.abb == attr(x, "geog")]
  pop <- attr(x, "total")

  cat("Weights Summary\n",
      paste0(geog, ": ", year, "\n"),
      "Adult Pop: ", format(pop, big.mark = ","),
      "\n=====================================\n",
      sep = "")

  print.data.frame(x %>% select(-state, -year), row.names = F)
}
