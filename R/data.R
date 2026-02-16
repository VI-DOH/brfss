

#' Has Some Valid Data
#'
#' Inspects column to see if any values are not NA
#'
#' @param df_brfss - data.frame: dataset containing column of interest
#' @param coi - character: column of interest
#'
#' @return logical: TRUE if column contains at least 1 non-NA value
#' @export
#'
#' @examples
#' dontrun{
#' if(has_data(coi = "DIABETE4")) df_stats <- survey_stats("DIABETE4", pct = TRUE)
#' }
#'
has_data <- function(df_brfss = NULL, coi) {

  if(is.null(df_brfss)) df_brfss <- brfss::brfss_data()

  x <- df_brfss %>% pull({{coi}})
  !all(is.na(x))
}

resolve_column <- function(df_brfss = NULL, coi) {

  if(is.null(df_brfss)) return(NULL)

  cnames <- df_brfss %>% colnames()
  cois <- grep(coi,cnames, value = TRUE)

  if(length(cois) == 0) {
    message(paste0("No match for <", coi, ">" ))
    return(NULL)
  }

  if(length(cois) > 1) {
    message(paste0("More than 1 match for <", coi, "> ... ", paste0(cois, collapse = ", ")))
    return(NULL)
  }

  cois
}

#' Has a Specified Column
#'
#' @param df_brfss - data.frame: dataset containing column of interest
#' @param coi - character: column of interest
#'
#' @return logical: TRUE if column exists
#' @export
#'
#' @examples
#' dontrun{
#' if(has_column(coi = "DIABETE4")) df_stats <- survey_stats("DIABETE4", pct = TRUE)
#' }
#'
has_column <- function(df_brfss = NULL, coi) {

  if(is.null(df_brfss)) df_brfss <- brfss::brfss_data()

  coi %in% (df_brfss %>% colnames())

}

#' File Patterns for Creating Standard paths
#'
#' A dataset containing the information for creating file patterns used to build standardized
#' paths to folders and files. This package uses these paths to standardize the locations
#' for users, and to remove the need to constantly pass complicated paths to each function.
#'
#' Common variables to use are YEAR and GEOG. If you pass YEAR to function that applies a pattern, the
#' variable [YR] (2-digit) will be created internally. Year should be a 4-digit integer.
#'
#' There is a hierarchy built in so that one named pattern such as a folder can be
#' inside another's pattern.
#'
#' There is also a logical component that allows different patterns to be used when different
#' conditions are met (e.g. source is 'ascii' or 'sas')
#'
#' Examples ...
#'
#' name                                             pattern
#' =============================================================================================
#' data_folder                                      ./data/
#' raw_data_folder                                  ./data_raw/
#' output_folder                                    ./output/
#' brfss_raw_data_folder                            {raw_data_folder}
#' brfss_annual_raw_data_folder                     {brfss_raw_data_folder}[YEAR]/
#' brfss_data_folder                                {data_folder}
#' brfss_annual_data_folder                         {brfss_data_folder}[YEAR]/
#' codebook_layout_folder                           {brfss_annual_raw_data_folder}
#' codebook_layout_file                             codebook[YR]_llcp
#' codebook_layout_ext                              pdf
#' layout_folder                                {brfss_annual_data_folder}layout/
#' sas_layout_file                                  layout_[YEAR].RData
#' sas_layout_path                                  {layout_folder}{sas_layout_file}
#'
#' @format A data frame with 4 variables:
#' \describe{
#'
#'   \item{name}{name of pattern to reference}
#'   \item{pattern}{text of pattern nformation with variables embedded in square brackets. e.g "./data/[YEAR]/}
#'   \item{group}{text similar to name, but used to link multiple patterns. e.g. to have a set of paths to download}
#'   \item{desc}{description of pattern use}
#'   ...
#' }
"naming_patterns"


#' Storage for Default Year and State/Geography
#'
#' A list containing  information for current BRFSS processing
#'
#' @format A data frame with 3 variables and 54 rows
#' \describe{
#'
#'   \item{Geog}{text name of geography (e.g. "Montana")}
#'   \item{Id}{BRFSS integer ID for the Geography - equivalent to the FIPS code (e.g. 30)}
#'   \item{Abbrev}{2-digit abbreviation for the geography (state/territory) (e.g. "MT")}
#'   ...
#' }
"geogs"

#' ' Aliases for Column Names
#' #'
#' #' A list containing  information for current BRFSS processing
#' #'
#' #' @format A data frame with 3 variables
#' #' \describe{
#' #'
#' #'   \item{geog}{text name of geography (e.g. "Montana")}
#' #'   \item{column}{BRFSS integer ID for the Geography - equivalent to the FIPS code (e.g. 30)}
#' #'   \item{alias}{2-digit abbreviation for the geography (state/territory) (e.g. "MT")}
#' #'   ...
#' #' }
#' "aliases"
