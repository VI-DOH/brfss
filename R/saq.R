




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
build_saq_layout<-function(year=NULL, geog=NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)

  saq_filename_csv <- apply.pattern("saq_raw_path", YEAR = year, GEOG = geog, EXT = "local")
  saq_layout <- apply.pattern("saq_layout_path", YEAR = year, GEOG = geog, EXT = "local")

  df_layout_saq<-read.csv(saq_filename_csv)

  save(df_layout_saq,file = saq_layout)

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
saq_layout<-function(year = NULL, geog = NULL){

  year <- get.year(year)
  geog <- get.geog(geog)

  saq_layout <- apply.pattern("saq_layout_path", YEAR = year, GEOG = geog)
  orrr::get.rdata(saq_layout)
}



#' Merge State Specific Questions with Standard Layout
#'
#' @param year
#' @param geog
#' @param df_layout
#' @param df_saq_layout
#'
#' @return
#' @export
#'
#' @examples
merge_saq_layout <- function(year = NULL, geog = NULL, df_layout = NULL, df_saq_layout = NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)

  if(is.null(df_layout)) {
    df_layout <- get.codebook.layout(year)
    if(is.null(df_layout)) df_layout <- get.sas.layout(year)
  }

  if(is.null(df_saq_layout)) {
    df_saq_layout <- saq_layout(year)
  }

  if(!is.null(df_saq_layout)) {


    df_saq_layout <- df_saq_layout  %>%
      mutate(sect_num = as.character(sect_num)) %>%
      mutate(question_num = as.character(question_num))

    slice_row <- which(df_layout$col_name == 'STATEQUE')
    saq_size <-  df_layout %>% slice(slice_row) %>% pull(field_size)

    keep0 <- df_layout %>% slice(1:(slice_row-1))
    keep1 <- df_layout %>% slice((slice_row+1):nrow(df_layout))

    my_saq_size <-df_saq_layout  %>% pull(field_size) %>% sum()

    saq_dummy <- data.frame( col_name = 'DUMMY_SAQ', field_size = saq_size - my_saq_size)

    df_layout <- keep0  %>%
      bind_rows(df_saq_layout, saq_dummy, keep1)
  }

  fldr <- apply.pattern("codebook_layout_folder", YEAR = year, GEOG = geog)
  fil <- apply.pattern("merged_layout_file", YEAR = year, GEOG = geog)

  file <- paste0(fldr,fil)

  if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


  save(df_layout, file = file)

  invisible()

}


#' Build State-Added-Questions Values File
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
#' build_saq_values(2021)
#' }
build_saq_values<-function(year=NULL, geog=NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)

  saq_filename_csv <- apply.pattern("saq_raw_values_path", YEAR = year, GEOG = geog, EXT = "local")
  saq_values_path <- apply.pattern("saq_values_path", YEAR = year, GEOG = geog, EXT = "local")

  df_values_saq<-read.csv(saq_filename_csv, check.names = FALSE, fileEncoding="UTF-8-BOM") %>%
    mutate(value = as.character(value))

  save(df_values_saq,file = saq_values_path)

}

#' Merge State Specific Question Values with Standard Question Values
#'
#' @param year
#' @param geog
#' @param df_values
#' @param df_saq_values
#'
#' @return
#' @export
#'
#' @examples
merge_saq_values <- function(year = NULL, geog = NULL, df_values = NULL, df_saq_values = NULL) {

  year <- get.year(year)
  geog <- get.geog(geog)

  if(is.null(df_values)) {
    df_values <- codebook_values(year)
  }

  if(is.null(df_saq_values)) {
    df_saq_values <- saq_values(year)
  }

  if(!is.null(df_saq_values)) {

    df_values_mrg <- df_values  %>%
      bind_rows(df_saq_values)
  }

  fldr <- apply.pattern("codebook_layout_folder", YEAR = year, GEOG = geog)
  fil <- apply.pattern("merged_values_file", YEAR = year, GEOG = geog)

  file <- paste0(fldr,fil)

  if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


  save(df_values_mrg, file = file)

  invisible()

}

#' Get State-Added-Question Values
#'
#' @param year integer year of interest
#' @param geog character geog of interest (or none, the default geography)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df_saq_values <- saq_values(2021)
#' }
saq_values<-function(year = NULL, geog = NULL){

  year <- get.year(year)
  geog <- get.geog(geog)

  saq_values <- apply.pattern("saq_values_path", YEAR = year, GEOG = geog)
  orrr::get.rdata(saq_values)
}

