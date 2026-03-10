
#' Get BRFSS Survey Data
#'
#' @param year - int - year of interest
#' @param geog - character - 2-char geog of interest (ex: "NY")
#' @param version - survey version; 0 (default) = main survey
#' @param extent - character - extent of data ("local" or public")
#' @param source - character - source of data ("ascii" or sas")
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- brfss_data(2020, geog = "MD", version = 1)
#'
#' }
#'
brfss_data <- function(...) {


  brfss.params(...)
  params <- my.brfss.patterns()

  fname <- brfss_data_path(rw = 'r')

  if(is.null(fname))  return(NULL)

  df_brfss<- readRDS(fname)

  params <- my.brfss.patterns()

  structure(df_brfss,
            class = c("brfss_data", "data.frame"),
            year = params["YEAR"] %>% as.integer() %>% unname(),
            geog = params["GEOG"] %>% unname(),
            source = params["SRC"] %>% unname(),
            extent = params["EXT"] %>% unname(),
            version = params["VERS"] %>% unname()
  )
}

#' Subset method for brfss_data
#'
#' @param x A brfss_data object
#' @param i Row indices
#' @param j Column indices
#' @param drop Logical, whether to drop dimensions
#' @return Subsetted brfss_data object with attributes preserved
#' @export
#' @method [ brfss_data
`[.brfss_data` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod("[")

  # Preserve attributes
  attrs <- attributes(x)
  keep <- setdiff(names(attrs), c("names", "row.names", "class"))
  for (nm in keep) attr(out, nm) <- attrs[[nm]]

  if (!drop) class(out) <- class(x)
  out
}


#' Subset method for brfss_data
#'
#' @param x A brfss_data object
#' @return Summary for brfss_data object
#' @export
#' @method summary brfss_data
summary.brfss_data <- function(df) {

  x <- brfss:::responses() %>% t() %>% as.data.frame() %>% rename(` ` = 1)
  print(x)

  x <- brfss::get_module_stats() %>% select(Module = mod_num, Name = module)
  nc <- (x %>% pull(Name) %>% nchar() %>% max()) + 8

  cat("\n", rep("=", nc), "\n",
      "  Modules\n", rep("=", nc), "\n", sep = "")

  print(x, row.names = F)
}

#' Get BRFSS Geography-Based Survey Data
#'
#' Force the data retrieval to be for a geography of interest.

#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- brfss_data(2020, geog = "MD", version = 1)
#'
#' }
#'
brfss_geog_data <- function() {

  #  extent <- brfss.param(extent)

  #  brfss.param(extent = "local")

  fname <- brfss_data_path(rw = 'r')

  if(is.null(fname))  return(NULL)

  df_brfss<- readRDS(fname)

  # brfss.param(extent = extent)

  df_brfss
}

coi_data_vers<- function(df_data = NULL, coi=NULL, subvars = NULL, version = NULL) {

  stratum <- apply.pattern("stratum_col")

  brfss.params(version = version)
  if(is.null(df_data)) df_data <- brfss_geog_data()


  if(is.null(df_data)) return(data.frame())

  df_data %>%
    dplyr::rename(STRATUM = {{stratum}}) %>%
    dplyr::select(all_of(coi), FINAL_WT, STRATUM, any_of(subvars)) %>%
    dplyr::mutate(vers = {{version}})

}

coi_data <- function(df_data=NULL, coi = NULL, subvars = NULL, exclude = "^$",
                     weight_col = "FINAL_WT") {


  #df_brfss <- brfss_data(year,geog)
  df_brfss <- data.frame() #   coi_data(coi, year,geog,version = 0)

  # invisible(
  #   sapply(0:highest_version(),function(ver) {
  #     browser()
  #     df_brfss <<- df_brfss %>%
  #       bind_rows(coi_data_vers(df_data = df_data,
  #                               coi = coi, subvars = subvars,
  #                               version = ver))
  #   })
  # )


  df_brfss <- purrr::map(0:highest_version(),function(ver) {
    coi_data_vers(df_data = df_data,
                  coi = coi, subvars = subvars,
                  version = ver)
  })


  df_brfss <- df_brfss %>%
    bind_rows()



  voi <- df_brfss %>% pull(vers) %>% unique()

  df_resp <- responses_by_geog()

  if(!is.null(df_resp)) {
    df_resp <- df_resp %>%
      dplyr::filter(version %in% voi) %>%
      dplyr::mutate(pct = responses/sum(responses))
  } else {
    df_resp <- data.frame(version = 0, pct = 1.0)
  }

  df_brfss <- df_brfss %>%
    dplyr::left_join(df_resp, by = c("vers" = "version")) %>%
    dplyr::mutate(FINAL_WT = FINAL_WT * pct) %>%
    dplyr::select(matches(coi), FINAL_WT, STRATUM, any_of(subvars))%>%
    dplyr::rename(coi = {{coi}})%>%
    dplyr::mutate(coi = replace(coi, grep(exclude,coi),NA)) %>%
    filter(!is.na(coi))

  if (is.factor(df_brfss %>% pull(coi)))
    df_brfss <- df_brfss %>% dplyr::mutate(coi = droplevels(coi))

  df_brfss %>%
    dplyr::rename({{coi}} := coi)
}

