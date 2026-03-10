#' Add or update attributes on an object
#'
#' This function merges a set of new attributes with any existing ones
#' on an object, ensuring that existing attributes not being replaced
#' are preserved. It also automatically tags the object as both
#' `is_calculated = TRUE` and `is_custom = TRUE`.
#'
#' @param x An R object to which attributes will be added or updated.
#' @param new_attribs A named list of attributes to add or update on `x`.
#'
#' @return The same object `x`, with updated attributes.
#'
#' @examples
#' x <- 1:5
#' new_attrs <- list(source = "BRFSS", year = 2023)
#' y <- add_prep_attrs(x, new_attrs)
#' attributes(y)
#'
#' @export
add_prep_attrs <- function(x, new_attribs) {

  old_attribs <- attributes(x)

  old_names <- names(old_attribs)
  new_names <- names(new_attribs)
  keep <- setdiff(old_names, new_names)

  old_attribs <- old_attribs[keep]

  all_attribs <- c(old_attribs, new_attribs, list(is_calculated = TRUE, is_custom = TRUE))
  attributes(x) <- all_attribs

  x
}

#' Create a BRFSS-labeled factor
#'
#' This function converts a vector to a factor, applying any arguments
#' accepted by [base::factor()], and assigns the additional class `"brfss"`.
#' The `"brfss"` class can be used for custom printing, labeling, or
#' specialized handling of BRFSS-coded variables.
#'
#' @param x A vector to be converted to a factor.
#' @param ... Additional arguments passed to [base::factor()].
#'
#' @return A factor with an additional `"brfss"` class.
#'
#' @examples
#' x <- c("Yes", "No", "Yes", "Don't know")
#' f <- factor_brfss(x, levels = c("Yes", "No", "Don't know"))
#' class(f)
#'
#' @export
factor_brfss <- function(x, ...) {

  f <- factor(x, ...)
  class(f) <- c("brfss", class(f)) %>% unique()
  f
}

#' Prepped BRFSS Data
#'
#'   This will either add or modify columns of BRFSS data. Useful for creating custom calculated variables
#'
#' @param ... parameters for brfss.params()
#'
#' @returns - data frame with BRFSS data added or modified
#' @export
#'
#' @examples
prepped_data <- function(...) {

  brfss.params(...)
  #if(year == 2024) browser()

  params <- as.list(brfss::brfss.params())

  func_any <- paste0("prepped_", params$year)
  func_geog <- paste0("prepped_", params$year, "_", params$geog)

  df <- brfss_data()

  if(!is.null(df)) f <- sapply(df, function(col) {is.factor(col)}) %>% which()

  if(is.null(df) || length(f) == 0) {

    source <- brfss.param(source)
    try_source <- ifelse (source == "ascii", "sas", "ascii")

    brfss.param(source = try_source)

    df <- brfss_data()

    brfss.param(source = source)
  }

  if(is.null(df))  return(NULL)

  attribs <-  attributes(df)
  attribs <-  attribs[!names(attribs) %in% c("row.names", "names", "class")]

  if(func_any %in% ls(.GlobalEnv)) {
    df <- do.call(func_any, args = list(df))
  }

  if(func_geog %in% ls(.GlobalEnv)) {

    #browser()
    df <- do.call(func_geog,args = list(df))
  }

  do.call(structure, c(list(df, class = c("brfss_prepped", "data.frame")), attribs))
}

#' Subset method for brfss_data
#'
#' @param x A brfss_prepped object
#' @return Summary for brfss_prepped object
#' @export
#' @method summary brfss_prepped
summary.brfss_prepped <- function(df) {

  x <- brfss:::responses() %>% t() %>% as.data.frame() %>% rename(` ` = 1)
  print(x)

  cat("\n",
      "=======================================\n",
      "  Modules\n",
      "=======================================\n")

  x <- brfss::get_module_stats() %>% select(Module = mod_num, Name = module)
  print(x, row.names = F)
}

