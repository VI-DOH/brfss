
alias.file<-function() {

  pttrns <- my.brfss.patterns()

  apply.pattern("aliases_path", pttrns)


}

aliases <- function() {

  readRDS(alias.file())
}



save.aliases <- function(df_alias) {

  saveRDS(df_alias, file = alias.file())
}

alias.df.name<-function() {

  return("df_alias")

}

#' Refresh Aliases
#'
#' Reset the aliases data set to the default
#'
#' @return
#' @export
#'
#' @examples
refresh.aliases <- function() {

  file <- alias.file()

  if(file.exists(file)) {
    file.remove(alias.file())
  }

  data("aliases",package="brfss")
  saveRDS(df_alias,file)
}

#####################################################################
##
##    this is run on the package side

init.alias <- function() {

  file <- "./data/aliases.rda"

  if(file.exists(file)) {
    file.remove(alias.file())
  }

  df_alias<-data.frame(column = character(0), alias = character(0), geog = character(0))
  save(df_alias,file = file)
}

#' Remove Alias
#'
#' Remove an alias from the aliases data set
#'
#' @param column
#'
#' @return
#' @export
#'
#' @examples
remove.alias<-function(column) {



  df<- aliases()
  nrows<-nrow(df)

  df <- df %>% filter(!column %in% {{column}})
  save.aliases(df)
}

#' Put Alias
#'
#' Put an alias in the aliases data set
#'
#' @param column
#' @param alias
#'
#' @return
#' @export
#'
#' @examples
put.alias<-function(columns,aliases, geogs = "", replace = FALSE) {

  df<-aliases()

  if(nrow(df %>% filter(column %in% {{columns}})) > 0) {
    if(TRUE) {
      cat("Warning ... these aliases already exist:",
          paste0(alias[alias %in% df[["alias"]]], collapse = ", "),"\n")

      return(0)
    }
  }

  df_add <- data.frame(column,alias,geog)

  df<-rbind(df[!df$column%in%column,],df_add)
  save.aliases(df)

}

#' Get Alias
#'
#' Get an alias from the aliases data set
#'
#' @param columns
#'
#' @export
get.alias<-function(columns=NULL) {

  aliases() %>% filter(column %in%{{columns}}) %>% pull(alias)

  # if(missing(columns)) {
  #   ret<-df$column
  # } else {
  #   ret<-sapply(columns, function(column) {
  #     df<-df[df$column==column,]
  #     if(nrow(df)>0) {
  #       al<-df$alias
  #     } else {
  #       al<-column
  #     }
  #     al
  #   })
  #
  # }
  # ret
}


use_aliases <- function(df) {


  df_alias <- orrr::get.rdata(alias.file())

  mapply(function(col, alias) {

    df <<- df %>% rename( {{alias}} := contains(col))

  }, df_alias$column, df_alias$alias)

  df
}

