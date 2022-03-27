




#' Build State-Added-Questions Layout File
#'
#' Read in .csv file and save as .rda file
#'
#' @param year integer year of interest
#' @param geog character geog of interest (or none, the default geography)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build_saq_layout(2021)
#' }
build_saq_layout<-function(year, geog="") {
  saq_filename_csv <- apply.pattern("saq_layout_raw", YEAR = year, GEOG = geog)
  saq_layout <- apply.pattern("saq_layout", YEAR = year, GEOG = geog)

  df_saq<-read.csv(saq_filename_csv)

  save(df_saq,file = saq_layout)

}

#' Get State-Added-Questions Layout
#'
#' @param year integer year of interest
#' @param geog character geog of interest (or none, the default geography)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df_saq_layout <- saq_layout(2021)
#' }
saq_layout<-function(year,geog=""){

  saq_layout <- apply.pattern("saq_layout", YEAR = year, GEOG = geog)
  orrr::get.rdata(saq_layout)
}

