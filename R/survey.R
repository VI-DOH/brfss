
#' Get BRFSS Survey Data
#'
#' @param year - int - year of interest
#' @param geog - character - 2-char geog of interest (ex: "NY")
#' @param version - survey version; 0 (default) = main survey
#' @param extent - character - extent of data ("local" or national")
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
brfss_data <- function() {

  params <- my.brfss.patterns()

  fname <- brfss_data_path(rw = 'r')

  if(is.null(fname))  return(NULL)

  df_brfss<- readRDS(fname)

  df_brfss
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

  extent <- brfss.param(extent)
  brfss.param(extent = "local")

  fname <- brfss_data_path(rw = 'r')

  if(is.null(fname))  return(NULL)

  df_brfss<- readRDS(fname)

  brfss.param(extent = extent)

  df_brfss
}

#' Value Representing Binary No
#'
#' Retrieve the value used to represent 'No' in binary (Yes/No) calculations.
#' When there are more than 2 possible values in the denominator and/or numerator,
#' then it is convenient to convert all values in a numerator to Yes/No
#'
#' @return integer - value representing 'No'
#' @export
#'
binary_no<-function() {
  return(-2)
}

#' Value Representing Binary Yes
#'
#' Retrieve the value used to represent 'Yes' in binary (Yes/No) calculations.
#' When there are more than 2 possible values in the denominator and/or numerator,
#' then it is convenient to convert all values in a numerator to Yes/No
#'
#' @return integer - value representing 'Yes'
#' @export
#'
binary_yes<-function() {
  return(-1)
}


coi_data_vers<- function(df_data = NULL, coi=NULL, subsets = NULL, version = NULL) {

  vwt <- apply.pattern("weight_col",VERS=version)
  stratum <- apply.pattern("stratum_col")

  brfss.params(version = version)
  if(is.null(df_data)) df_data <- brfss_geog_data()

  if(is.null(df_data)) return(data.frame())

  df_data %>%
    rename(FINAL_WT = {{vwt}}) %>%
    rename(STRATUM = {{stratum}}) %>%
    select(all_of(coi), FINAL_WT, STRATUM, all_of(subsets)) %>%
    mutate(vers = {{version}}) %>%
    na.exclude()


}

coi_data <- function(df_data=NULL, coi = NULL, subsets = NULL, exclude = "^$") {


  #df_brfss <- brfss_data(year,geog)
  df_brfss <- data.frame() #   coi_data(coi, year,geog,version = 0)

  invisible(
    sapply(0:highest_version(),function(ver) {
      df_brfss <<- df_brfss %>%
        bind_rows(coi_data_vers(df_data = df_data,
                                coi = coi, subsets = subsets,
                                version = ver))
    })
  )

  voi <- df_brfss %>% pull(vers) %>% unique()

  df_resp <- responses_by_geog(year,geog) %>%
    filter(version %in% voi) %>%
    mutate(pct = responses/sum(responses))

  df_brfss <- df_brfss %>%
    left_join(df_resp, by = c("vers" = "version")) %>%
    mutate(FINAL_WT = FINAL_WT * pct) %>%
    select(all_of(coi), FINAL_WT, STRATUM, all_of(subsets))%>%
    rename(coi = {{coi}})%>%
    mutate(coi = replace(coi, grep(exclude,coi),NA)) %>%
    na.exclude() %>%
    mutate(coi = droplevels(coi)) %>%
    rename({{coi}} := coi)


  df_brfss
}

