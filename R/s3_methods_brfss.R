#' Print and format methods for 'brfss' class
#'
#' These methods define how objects of class `"brfss"` are displayed
#' in the console and within data frames or tibbles.
#'
#' The `"brfss"` class is a subclass of `"factor"`. These methods extend
#' the base printing and formatting behavior to make `"brfss"` objects
#' easier to read while preserving their class attributes.
#'
#' @name brfss-print-format
#' @keywords internal
#' @seealso [base::print()], [base::format()]
#' @examples
#' x <- structure(factor(c("Yes", "No", NA)), class = c("brfss", "factor"))
#' print(x)
#' format(x)
NULL


#' @rdname brfss-print-format
#' @export
format.brfss <- function(x, ...) {
  # Return as character for readability in tibbles/data.frames
  as.character(x)
}


#' @rdname brfss-print-format
#' @export
print.brfss <- function(x, ...) {

  attrs <- attributes(x)

  cat("==============================\n",
      attrs[["variable"]], "\n",
      "class: brfss\n",
      "-----------------------------------\n",
      attrs[["section_type"]], " #",
      attrs[["section_num"]],"\n",
      attrs[["section_name"]],"\n",
      "-----------------------------------\n",
      "Population: ", attrs[["population"]],"\n",
      sep = "")

  if(attrs[["is_custom"]]) {
    cat(" ***  Custom Variable  ***\n", sep = "")
  } else if("is_saq" %in% names(attrs) && attrs[["is_saq"]]){
    cat(" ***  State-added  ***\n", sep = "")
  } else {

  }

  cat("\n", sep = "")

  print(table(x, dnn = attrs[["label"]]) )

}


#' vctrs methods for the 'brfss' class
#'
#' These methods define how objects of class `"brfss"` interact with other
#' vectors in the vctrs framework (used by dplyr, tidyr, etc.).
#'
#' The `"brfss"` class is a subclass of `"factor"`. These methods ensure that
#' `"brfss"` objects can be combined, cast, and otherwise manipulated without
#' causing type errors.
#'
#' @name vctrs-methods-brfss
#' @keywords internal
#' @seealso [vctrs::vec_ptype2()], [vctrs::vec_cast()]
#' @examples
#' x <- structure(factor(c("Yes", NA)), class = c("brfss", "factor"))
#' y <- structure(factor(c(NA, "No")), class = c("brfss", "factor"))
#'
#' # vctrs-aware combination
#' dplyr::if_else(is.na(x), y, x)
NULL


#' @rdname vctrs-methods-brfss
#' @method vec_ptype2 brfss brfss
#' @export
vec_ptype2.brfss.brfss <- function(x, y, ...) {
  structure(factor(), class = c("brfss", "factor"))
}

#' @rdname vctrs-methods-brfss
#' @method vec_ptype2 brfss factor
#' @export
vec_ptype2.brfss.factor <- function(x, y, ...) {
  factor()
}

#' @rdname vctrs-methods-brfss
#' @method vec_ptype2 factor brfss
#' @export
vec_ptype2.factor.brfss <- function(x, y, ...) {
  factor()
}


#' @rdname vctrs-methods-brfss
#' @method vec_cast brfss brfss
#' @export
vec_cast.brfss.brfss <- function(x, to, ...) {
  x
}

#' @rdname vctrs-methods-brfss
#' @method vec_cast factor brfss
#' @export
vec_cast.factor.brfss <- function(x, to, ...) {
  factor(x)
}

#' @rdname vctrs-methods-brfss
#' @method vec_cast brfss factor
#' @export
vec_cast.brfss.factor <- function(x, to, ...) {
  class(x) <- c("brfss", "factor")
  x
}
