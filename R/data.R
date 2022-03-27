

#' File Patterns for Creating Standard paths
#'
#' A dataset containing the information for creating file patterns used to build standardized
#' paths to folders and files. This package uses these paths to standardize the locations
#' for users, and to remove the need to constantly pass complicated paths to each function.
#'
#' Common variables to use are YEAR and GEOG. If you pass YEAR to function that applies a pattern, the
#' variable [YR] (2-digit) will be created internally. Year should be a 4-digit integer.
#'
#' There is a hierarchy built in so that one named path such as a folder can be inside another's pattern.
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
#' brfss_geog_folder                                {brfss_data_folder}[YEAR]/geog/
#' brfss_geog_file                                  [GEOG]_[YEAR].RData
#' brfss_geog_file_version                          [GEOG]_[YEAR]_V[VERS].RData
#' brfss_geog_path                                  {brfss_geog_folder}{brfss_geog_file}
#' codebook_layout_folder                           {brfss_annual_raw_data_folder}
#' codebook_layout_file                             codebook[YR]_llcp
#' codebook_layout_ext                              pdf
#' brfss_layout_folder                              {brfss_annual_data_folder}layout/
#' brfss_layout_file                                layout_[YEAR].RData
#' brfss_layout_path                                {brfss_layout_folder}{brfss_layout_file}
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
"file_patterns"
