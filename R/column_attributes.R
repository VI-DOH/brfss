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

  df_layout<- get.layout()

  if(!"saq" %in% colnames(df_layout)) {
    df_layout$saq <- NA
  }

  mapply(function(lbl,col_name,typ,n,i,nm,qu,calc, saq) {

    #    df_vals <- df_values %>% filter(col_name == {{col_name}})
    if(!is.null(df_in[[col_name]])) {
      if(is.na(typ) || is.null(typ)) typ <- ""
      if(is.na(n) || is.null(n)) n <- ""
      if(is.na(i) || is.null(i)) i <- ""
      if(is.na(nm) || is.null(nm)) nm <- ""
      if(is.na(qu) || is.null(qu)) qu <- ""
      if(is.na(calc) || is.null(calc)) qu <- ""
      if(is.na(saq) || is.null(saq)) saq <- ""

      atts <-  c(
        "section_type" = typ,
        "section_num" = n,
        "section_index" = i,
        "section_name" = stringr::str_trim(nm),
        "label" = lbl,
        "question" = qu,
        "variable" = col_name,
        "is_calculated" = as.logical(calc),
        "is_custom" = FALSE,
        "is_saq" = as.logical(saq)
      )


      df_in <<- df_in %>% add_attributes({{col_name}}, atts = atts)
      class(df_in[[col_name]]) <<- c("brfss", class(df_in[[col_name]]))

    } else {

    }

  }, df_layout$label, df_layout$col_name, df_layout$sect_type, df_layout$sect_num,
  df_layout$question_num,df_layout$section, df_layout$question, df_layout$calculated,
  df_layout$saq)

  df_in

}

add_brfss_class <- function(x, ...) {

  f <- factor(x, ...)
  class(f) <- c("brfss", class(f))
  f
}

quosures <- function(...) {

  quos <- quos(..., .ignore_empty = "all")

  ret <- sapply(quos, function(quo) {

    expr <- rlang::quo_get_expr(quo)

    as.character(expr)
  })
  names(ret) <- names(quos)

  ret
}

#' Add a List of Attributes to a column
#'
#' Adds attributes to a column from a named list
#'
#'  The standard attributes for a BRFSS column are:
#'   section type: Core, Module, SAQ, Non-Survey
#'   section num: Section number within the section type
#'   section index: the index of this column within the section
#'   section name: Name of the section
#'   label: Label assigned to this column
#'   question: The actual question asked (if this was a survey question)
#'   variable: Variable name
#'
#' @param df - a data.frame containing the column
#' @param col - column name
#' @param atts - list of attributes
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' df <- brfss_data()
#'
#' atts <- c(
#' "section_type" = "Core",
#' "section_num" = 5,
#' "section_index" = 6,
#' "section_name" = "Cancer",
#' "label" = "Do you have cancer?",
#' "question"="Has a PCP ever told you that you have cancer",
#' "variable"="CANCER"

#' )

#' df <- df %>% add_attributes("TESTME", atts)
#' }
#'
add_attributes <- function(df, ... , atts) {

  col <- as.character(quosures(...))[1]

  mapply(function(att, nm) {
    #browser()

    tryCatch(
      {
        if(is.null(att)) att <- ""
        attr(df[[col]], nm) <<- att
      },
      error = function(e) {

        cat("\n", e$message,"\n")
        cat("trying to set column [",col, "] attribute [", nm, "] to value [", att,"]\n", sep = "")
        if(is.null(df[[col]]))  cat(" ... column ", col, " = NULL", "\n\n", sep = "")
      }
    )
  }, atts, names(atts) )
  df
}

#' List of Standard BRFSS Attributes for a column
#'
#'  The standard attributes for a BRFSS column are:
#'   section type: Core, Module, SAQ, Non-Survey
#'   section num: Section number within the section type
#'   section index: the index of this column within the section
#'   section name: Name of the section
#'   label: Label assigned to this column
#'   question: The actual question asked (if this was a survey question)
#'   variable: Variable name
#' @return the attribute names
#' @export
#'

standard_attributes <- function(fmt = FALSE) {


  x <-   c(
    "section_type",
    "section_num",
    "section_index",
    "section_name",
    "label",
    "question",
    "variable",
    "calculated",
    "custom"
  )


  if(fmt) cat(x %>%
                paste0(collapse = " = '',\n") %>%
                paste0(" = ''\n" ) %>%
                paste0("c(\n", .,")")) else x

}


#' Fetch Column Names from Attributes
#'
#'   Get the column names that match a pattern on an attribute
#' @param df
#' @param attrib
#' @param pttrn
#'
#' @return character vector with matching column names
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # get the columns that are associated with the Diabetes section
#'
#' cols_from_attrib("section_name", "^Diab")
#' }
#'

cols_from_attrib <- function(df, attrib, pttrn) {

  ok <- sapply(1:ncol(df), function(icol) {

    att <- attributes(df %>% pull(icol))

    return(grepl(pttrn, att[attrib]))
  })

  cnames <- colnames(df)

  cnames[unlist(ok)]


}
