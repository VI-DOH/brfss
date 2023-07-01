#' Add Column Attributes
#'
#' Based on the layout structure from the codebook and/or the SAS layout file,
#' add the following attributes to BRFSS columns ...
#'
#'   section type: Core, Module, SAQ, Non-Survey
#'   section num: Section number within the section type
#'   section index: the index of this column within the section
#'   section name: Name of the section
#'   label: Label assigned to this column
#'   question: The actual question asked (if this was a survey question)

#'
#' @param layout
#' @param main
#' @param versions
#' @param verbose
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' add_column_attributes()
#' }
add_column_attributes<-function(layout = NULL, main = TRUE,
                                versions = TRUE, verbose = FALSE, progress = NULL) {

  show_progress(progress, message = "Add column attributes ... ")

  params <- my.brfss.patterns()

  if(is.null(layout)) {
    layout <- get.layout()
  }

  if(is.null(layout)) {
    winDialog("ok","Adding column attributes requires a layout.")
    return (NULL)
  }

  if(main) version <- 0 else {
    if(versions) version = 1 else return()
  }

  path <- apply.pattern("brfss_annual_data_path",params)

  while (file.exists(path)) {

    if(verbose) cat("... adding to version [", version, "] : ", path, "\n")

    show_progress(progress, message =
                    paste0("Adding atts to ... version [", version, "] : ", path))

    df <- brfss_data() %>%
      add_col_attributes()

    saveRDS(df, file = path)

    version <- version + 1
    brfss.param(version = version)
    params <- my.brfss.patterns()
    path <- apply.pattern("brfss_annual_data_path",params)

  }
  brfss.param(version = 0)

  invisible()
}

add_col_attributes <- function(df_in) {

  df_sasout<- get.layout()

  #browser()

  mapply(function(lbl,v,typ,n,i,nm,qu) {
    #cat(v,"|",typ,"|",n,"|",i,"|",nm)
    #browser()
    if(!is.null(df_in[[v]])) {
      if(is.na(typ) || is.null(typ)) typ<=""
      if(is.na(n) || is.null(n)) n<=""
      if(is.na(i) || is.null(i)) i<=""
      if(is.na(nm) || is.null(nm)) nm<=""
      if(is.na(qu) || is.null(qu)) qu<=""

      attr(df_in[[v]],"section_type")<<-typ
      attr(df_in[[v]],"section_num")<<-n
      attr(df_in[[v]],"section_index")<<-i
      attr(df_in[[v]],"section_name")<<-stringr::str_trim(nm)
      attr(df_in[[v]],"label")<<-lbl
      attr(df_in[[v]],"question")<<-qu
      attr(df_in[[v]],"variable")<<-v
    } else {

    }

  }, df_sasout$label, df_sasout$col_name, df_sasout$sect_type, df_sasout$sect_num,
  df_sasout$question_num,df_sasout$section, df_sasout$question)

  df_in

}
