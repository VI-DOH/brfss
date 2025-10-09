


#' Set Naming Pattern
#'
#' If the name exists already, it will overwrite (update) that name without warning.
#' If the name does not exist, it will add it.
#'
#'
#' @param name character: name of pattern
#' @param pattern character
#' @param group character
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' set.pattern(name = "raw_data_folder", pattern = "./data_raw/", desc = "Folder for raw BRFSS data")
#' }
#'
set.pattern <- function(name, pattern= NULL, group="", desc = "") {


  df_patterns <- get.patterns()

  if(is.null(pattern)) {
    return(NULL)
  }

  df_patterns <- df_patterns %>%
    filter(name != {{name}}) %>%
    bind_rows(data.frame(name , pattern, group, desc )) %>%
    distinct()

  save.patterns(df_patterns)

}

pattern.file.name <- function() {

  #paste0(here::here("data"),"/naming_patterns.rds")
  paste0("./data/naming_patterns.rds")
}

append.pattern <- function(df, name , pattern , type = "", group = "", desc = "") {

  #df0 <- data.frame(name  , pattern , group, desc)

  df %>%
    tibble::add_row( name, pattern , type, group, desc)
}

#' Move/Rename File to Standard Location
#'
#' Move a file (using file.rename()) to a location specified in the standard (expected)
#'  location using pattern naming.
#'
#' @param name character pattern name
#' @param file character file to be renamed
#' @param ... value pairs to be passed to the apply.pattern function
#'
#' @return logical status of operation
#' @export
#'
#' @examples
#' \dontrun{
#' relocate_by_pattern("ascii_path_raw", "./temp/ascii.dat", YEAR = 2021, VERS = 0)
#' }
relocate_by_pattern <- function(name, file, ...) {

  path_out <- apply.pattern(name, ...)

  dir_out <- dirname(path_out)

  if(!dir.exists(dir_out)) dir.create(dir_out)

  file.rename(from = file, to = path_out)
}


remove.patterns <- function(names) {


  df_patterns <- get.patterns() %>%
    filter(!name %in% names) %>%
    distinct()

  save.patterns(df_patterns)

}

#' File Pattern Names
#'
#' @param filter pattern (see grep) to apply to names
#'
#' @return character vector of pattern names
#' @export
#'
#' @examples
#' \dontrun{
#' # get file pattern names that have the word folder in them
#' pattern.names(filter = "folder")
#' }
#'
pattern.names <- function(filter=".*") {


  get.patterns() %>%
    filter(grepl(filter,name)) %>%
    pull(name)
}


#' Reset File Pattern Data
#'#'
#' @export
#'
refresh.patterns <- function() {

  naming_patterns <- pattern.file.name()
  if(file.exists(naming_patterns)) {
    file.remove(naming_patterns)
  }
  env <- new.env()
  data("naming_patterns", package = "brfss", envir = env )

  naming_patterns <- get(ls(env),envir = env)
  save.patterns(naming_patterns)

  invisible()
}

#' Get File Pattern Data
#'#'
#' @param names character pattern (see grep) to apply to names

#' @return data frame with all pattern data
#' @export
#'
get.patterns <- function(names = ".*") {

  naming_patterns <- readRDS(pattern.file.name())
  if(is.null(naming_patterns)) {
    env <- new.env()
    data("naming_patterns", package = getPackageName(), envir = env )

    naming_patterns <- get(ls(env),envir = env)
    save.patterns(naming_patterns)
  }

  naming_patterns %>%
    filter(grepl(names,.$name))
}

save.patterns <- function(naming_patterns) {

  suppressMessages({
    #usethis::use_data(naming_patterns, overwrite = TRUE)
    file <- paste0(rstudioapi::getActiveProject(),"/data/naming_patterns.rds")
    saveRDS(naming_patterns, file = file)
  })

}

expand.pattern <- function(pattern) {

  ctr <- 0
  while(grepl("\\$", pattern) && ctr<10) {
    pat0 <- gsub(".*?\\$(.*?)\\$.*","\\1",pattern)
    pat1<- get.pattern(pat0)
    pattern <- gsub(paste0("(\\$",pat0,"\\$)"),pat1,pattern)
    ctr <- ctr + 1

  }

  # get rid of double slashes (usually from substitution of other patterns)

  pattern <- gsub("([^:])//","\\1/",pattern)

  pattern
}

#' Get File Pattern
#'
#' Get the file pattern for a given name. If the name does not exist, it will return NULL
#'
#'
#' @param name character - name of pattern
#' @param expand logical - convert sub patterns
#'
#' @return character - the requested pattern
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern(name = "raw_data_folder")
#' }
#'
get.pattern <- function(name, expand = FALSE) {


  pat_name <- name

  df <- get.patterns()

  df <- df %>%
    filter(name=={{pat_name}})

  if(nrow(df) == 0) return(NULL)

  pttrn <- df %>%
    pull(pattern)

  if(expand) {
    pttrn <- expand.pattern(pttrn)
  }
  pttrn
}

#' Get File Pattern Group
#'
#' Get the file patterns for a given group. If the group does not exist, it will return NULL
#'
#'
#' @param group character: name of pattern group
#'
#' @return named character - the requested patterns with respective names
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern.group(group = "sas_downloads")
#' }
#'
get.pattern.group <- function(group) {

  naming_patterns <- get.patterns()

  naming_patterns <- naming_patterns  %>% filter(group=={{group}})

  patterns <- naming_patterns %>%
    pull(pattern)

  names(patterns) <- naming_patterns %>%
    pull(name)


  patterns

}

#' Get File Pattern Info
#'
#' Get the file pattern info (all fields) for a given name. If the name does not exist, it will return NULL
#'
#'
#' @param name character - name of pattern
#'
#' @return data.frame - the requested pattern info/fields (1 row)
#' @export
#'
#' @examples
#' \dontrun{
#' get.pattern(name = "raw_data_folder")
#' }
#'
get.pattern.info <- function(name) {

  naming_patterns <- get.patterns()

  naming_patterns %>% filter(name=={{name}})

}

#' Apply Variables to File Pattern
#'
#' User passes a pattern to use and the variable/value pairs to insert where specified in the pattern.
#' For example, if you store your annual data under the data folder by year, you might have a pattern named
#' 'annual_data_folder' that is './data/[YEAR]/' and you would call
#' apply.pattern("annual_data_folder", YEAR = 2020)
#'
#' There is one special case. If you pass YEAR as a variable, it will also create YR, and the
#' correesponding 2-digit value.
#'
#' @param name character - the pattern name
#' @param ... variables to insert
#'
#' @return character vector with variables inserted where needed
#' @export
#'
#' @examples
#' \dontrun{
#' sasout_file<-apply.pattern("sas_sasout_version",YEAR = year, VERS = version)
#'
#' }
#'
apply.pattern <- function(name,  ...) {

  args <- list(...)

  #req_params <- pattern.requirements(name) %>% pull(params)

  pats <- get.pattern(name = name)

  # if(length(req_params) == 1 && nchar(req_params) == 0) return(pats)

  # if(is.null(args) || length(args) == 0) {
  #
  #  return(NULL)
  # }

  pats <- sapply(pats, function(pat) {
    #pat <- get.pattern(name)
    expand.pattern(pat)
  })

  pats <- unname(pats)

  pats <- patternize(pats, args )

  pats

}

#############################################

#' Substitute Variables in Pattern
#'
#' @param strIn character - patterns to apply the variables to
#' @param ... list - variables to insert in form of c(YEAR = xxxx, VERS = 1), etc
#' @param expand logical - convert . or .. to a normalized path
#'
#' @return character vector of modified strings
#' @export
#'
#' @examples
#' year <- 2020
#' geog <- "MT"
#' patternize("XPT_[GEOG]_[YEAR]", YEAR = year, GEOG = geog)
#'
patternize<-function(strIn, ..., expand = TRUE) {

  args <- unlist(list(...))

  if(is.null(args)) return(strIn)

  #expand <- as.logical(args["expand"])
  ##  remove args with NULL value

  args <- args[!sapply(args,is.null)]

  nms <- names(args)
  vals <- unlist(unname(args))

  # ###########################################################################
  # ##
  # ##  this is a special case for YEAR ... creating the 2-digit year as well
  # yr_arg <- which(nms == "YEAR")
  #
  # if(length(yr_arg)>0) {
  #   year <- as.integer(vals[yr_arg[1]])
  #   nms[length(nms)+1] <- "YR"
  #   vals[length(vals)+1] <- year%%100
  #
  # }
  #
  # #############################################################################
  ret <- strIn

  mapply(function(nm,val) {
    nm <- paste0("^",nm,"^")
    ret<<-gsub(nm,val,ret,fixed = T)


  }, nms, vals)

  #######################################################################
  ##
  ##    check for logical expressions
  ##    e.g. "LLCP(^VERS^==0;^YEAR^)(^VERS^>0;^YR^V^VERS^)_XPT.zip"

  while(has_conditions(ret)) {

    # there is at least one condition .. get the text
    # cat("\n==========================================\n\n")
    # cat("ret (in)=",ret,"\n")

    next_cond <- next_condition(ret)

    # cat("next_cond$start=",next_cond$start,"\n")
    # cat("next_cond$end=",next_cond$end,"\n")

    # parse the expression part of the condition

    expr <- substr(ret, next_cond$start, next_cond$end)
    # cat("expr=",expr,"\n")

    # are there any embedded params ... ^XXXX^ ...
    if(grepl("^",expr, fixed = TRUE)) {

      expr <- ""
    } else {

      # condition is clean ... evaluate
      expr <- eval_pattern_cond(expr)
    }
    # cat("expr (new)=",expr,"\n")
    ret  <- paste0(substring(ret, 1,next_cond$start-1),
                   expr,
                   substring(ret, next_cond$end+1))

    # cat("ret=",ret,"\n")
  }

  # no more conditions

  if(expand && grepl("./",ret, fixed = T)) ret <- gsub("^.",here::here(),ret, fixed = T)
  #orrr::convert.dot(ret)

  # cat("ret (final)=",ret,"\n")

  ret
}

next_condition <- function(expr) {
  start <- max(gregexpr("{",expr, fixed  = T)[[1]])
  ends <- gregexpr("}",expr, fixed  = T)[[1]]
  tryCatch(
    end <- min(ends[ends>start]),
    warning = function(w){
      "Mismatched brackets"
    }
  )
  list(start = start, end = end)
}

has_conditions <- function(expr) {
  grepl(".*[{](.*)[}].*",expr)
}

eval_pattern_cond <- function(expr_in) {

  expr <- gsub(".*[{](.*)[}].*","\\1",expr_in)

  if(grepl(";",expr_in)) {
    expr <-  gsub("(.*);(.*)","\\1",expr)


    if(grepl(" *[=!]= *'",expr)) {
      expr <- gsub(" *([A-Za-z]*) *([=!]=)","'\\1' \\2",expr)
    }
    ok <- eval(parse(text = expr))

    if (ok) {
      ret <- gsub("(.*)[{].*;(.*)[}](.*)","\\1\\2\\3",expr_in)
    } else {
      ret <- gsub("(.*)[{].*;(.*)[}](.*)","\\1\\3",expr_in)
    }
  } else {
    ret <- eval(parse(text = expr))
  }
  ret
}



#' Try Patterns with Set of Arguments
#'
#' View all patterns with a given set of arguments. Useful for pattern naming testing.
#'
#' @param names character pattern to be applied to pattern names to subset result
#' @param ... arguments to be passed to apply.pattern()
#'
#' @return
#' @export
#'
#' @examples
#'\dontrun{
#' try.patterns("codebook", YEAR = 2018, GEOG = "MT", VERS = 0, EXT = 'local')
#'}
#'
try.patterns <- function(names = ".*", ...) {
  #
  pat_names <- pattern.names() %>% grep(names, ., value = TRUE)

  pats <- sapply(pat_names, function(nm) {

    pat<-apply.pattern(nm, ...)

    pat

  })


  df <- data.frame(name = pat_names, result = pats) %>% {rownames(.) <- NULL; .}


  df

}

#' Pattern Requirements
#'
#' Get the parameter requirements for naming pattern(s).
#' @param names
#'
#' @return data frame - pattern name and arguments required for the named patterns
#' @export
#'
#' @examples
#'\dontrun{
#' pattern.requirements("codebook")
#' }
#'
pattern.requirements <- function(names = ".*") {
  df_pats<- get.patterns(names)

  df <- data.frame()

  invisible(
    mapply(function(nm,pat) {

      pat <- expand.pattern(pat)

      pat0 <- unname(pat)
      pat0 <- gsub("^^","^ ^",pat0,fixed=TRUE) # helps sub work right

      params <- character(0)

      while(grepl("\\^", pat0)) {
        x <- gsub(".*?\\^(.*?)\\^.*","\\1",pat0)
        params <- c(params,x)
        pat0 <- sub("?\\^(.*?)\\^","", pat0)
      }
      params <- params %>% stringr::str_replace("YR","YEAR") %>% unique()

      df <<- df %>% bind_rows(data.frame(name = nm, params = paste0(params, collapse = ", ")))
    }, df_pats$name, df_pats$pattern)

  )


  df
}

#' Is Parameter Required?
#'
#' @param name character - name of the pattern
#' @param param character - parameter to test
#'
#' @return logical - TRUE if the parameter is required for the named pattern
#' @export
#'
#' @examples
#' \dontrun{
#' pat_name <- "sasout_download_file"
#' param <- "YEAR"
#' if (pattern.requires(pat_name. param) && is.null(param))
#'   warning(paste0("Parameter [", param, "] is required"))
#' }
pattern.requires <- function(name,param) {

  df_pats <- pattern.requirements(name)

  df_pats %>% pull(params) %>% {param %in% .}
}
