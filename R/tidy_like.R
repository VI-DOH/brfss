

copy_attrs <- function(df, ... ) {

  quosures <- quos(..., .ignore_empty = "all")

  quo <- quosures[[1]]
  quox <- quosures[1]
  to <- names(quox)
  expr <- rlang::quo_get_expr(quo)
  from <- as.character(expr)

  atts_from <- attributes(df[[from]])
  atts_to <- attributes(df[[to]])

  to_add <- names(atts_from)[!names(atts_from) %in% names(atts_to)]

  atts_to_add <- atts[to_add]

  attributes(df[atts_to])<-c(atts_to, atts_to_add)

  df

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


quo_var <- function(...) {

  quosures(...)[[1]]

}

quo_vars <- function(...) {

  quosures(...)

}


#######################################################
##
##              percents(df, ..., wt = NULL, my_where = NULL,
##                exclude = c("Don.{0,1}t ", "^Refuse"),
##                rm.NA = TRUE, digits = 2)

#' BRFSS Variable Percents
#'
#' @param df - brfss data
#' @param ...
#' @param wt - weighting variable
#' @param my_where - pattern for filter
#' @param exclude - pattern for responses to exclude
#' @param rm.NA - logical - remove NAs
#' @param digits - integer - number of digits
#' @param inc_n - logical - show 'n'
#' @param ft - logical - produce flextable
#'
#' @returns
#' @export
#'
#' @examples
percents <- function(df, ..., wt = NULL, my_where = NULL,
                     exclude = c("Don.{0,1}t ", "^Refuse"),
                     rm.NA = TRUE, digits = 2, inc_n = TRUE, ft = FALSE) {

  col_nm <- quosures(...)

  labels <- purrr::map(col_nm, ~paste0(attributes(df[[.x]])["variable"], ": ",
                                       attributes(df[[.x]])["label"])) %>% unlist()

  # titles <- c(paste0(as.character(brfss.param("year")), " BRFSS"))  # , labels)

  df_pcts <-  df

  if(!is.null(my_where)) {

    mapply(function(pttrn, col) {

      df_pcts  <<-  df_pcts %>% filter(grepl(pttrn,get(col)))
    }, my_where, names(my_where))
  }

  if(!is.null(exclude)) {

    sapply(col_nm, function(col) {
      sapply(exclude, function(pttrn) {
        df_pcts  <<- df_pcts %>% filter(!grepl(pttrn, .data[[col]]))
      })
    })
  }

  if(rm.NA) {

    sapply(col_nm, function(col) {
      df_pcts <<- df_pcts %>% filter(!is.na(.data[[col]]))
    })
  }

  x <- count(df_pcts, ..., wt = !!enquo(wt)) %>%
    mutate(pct = round(n/sum(n)*100, digits))%>%
    mutate(n = round(n, 0))

  if(nrow(x) == 0) return(NULL)

  if(!inc_n) x <- x %>% select(-n)

  if(ft) {

    #attrs <- x[[col_nm]] %>% attributes()

    ft <- flextable::flextable(x) %>%
      flextable::add_header_lines(titles) %>%
      flextable::align(i = 1:(nrow_part(., "header")-1), part = "header",
                       align = "center") %>%
      flextable::padding(padding.top = 2, padding.bottom = 1)

    return(ft)
  }

  structure(
    x,
    geog = attributes(df)["geog"],
    wt = rlang::get_expr(enquo(wt)) %>% as.character(),
    class = c("brfss_pcts", "data.frame")
  )

}

#' @rdname brfss_pcts-print-format
#' @export
print.brfss_pcts <- function(x, ...) {

  attrs <- attributes(x)
  xattrs <- attributes(x[[1]])

  geog <- attrs[["geog"]]

  geog <- brfss::geogs %>% filter(Abbrev == {{geog}}) %>% pull(Geog)

  cat("==============================\n",
      attrs[["year"]], " ", geog , " BRFSS\n",
      xattrs[["label"]], "\n",
      attrs$names[1], "\n",
      "weighted by:", attrs[["wt"]],"\n",
      "class: brfss_pcts\n",
      "-----------------------------------\n",
      "Population: ", attrs[["population"]],"\n",
      sep = "")


  cat("\n", sep = "")

  print(as.data.frame(x))

}
