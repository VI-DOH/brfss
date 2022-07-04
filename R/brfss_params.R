


require(dplyr)

#' Set/Get BRFSS Parameters
#'
#' Set or get a BRFSS parameter to be used in processing BRFSS data. Examples of this are year,
#' geography, and version. Arguments are symbols so the syntax is
#' brfss.param(year = 2021) to set and brfss.param(year) to get/fetch. The stored parameters can
#' include values to check against, and an event handler (on_change) to act upon a change in the value.
#'
#' @param ... pairs to save/set or parameter to get
#'
#' @return character - value of parameter (or nothing if set)
#' @export
#'
#' @examples
#' \dontrun{
#' brfss.param(geog = "MT")
#'
#' }
brfss.param <- function(..., myself = FALSE) {

  # get the arguments as unquoted

  arguments <- as.list(match.call())

  # first element is the function name itself ... if no params passed (length == 1), exit
  if(length(arguments)==1) return(NULL)

  # the argument of interest is the second one

  arg <- arguments[2]

  # if the argument is named, it is an assignment

  if(is.null(names(arg))) {

    # it is a retrieval ... get the value of the parameter
    my_brfss <- my.brfss.env()

    param <-as.character(arg[[1]])
    return(my_brfss[[param]]$value)

  } else {


    #   it is an assignment ... assign the value to the parameter

    #   get the actual value passed (as opposed to the symbol)

    arg_lst <- list(...)

    param <- names(arg)
    value <- arg_lst[[1]]
    set_param(param, value, myself)

  }

}

set_param <- function(param, value, myself = FALSE) {

  my_brfss <- my.brfss.env()

  # check to see if any legal values ...

  values <- my_brfss[[param]]$values
  if(!is.null(values)) {
    value <- match.arg(value,values)

    if(!value%in%values) {
      warning(paste0("The value for ", param, "[", value,"] is not in the list of acceptable values"))
      return(NULL)
    }
  }

  my_brfss[[param]]$value <- value
  my.brfss.save(my_brfss)

  # check to see if any events ...

  if(!is.null(my_brfss[[param]]$on_change) & !myself) {
    do.call(my_brfss[[param]]$on_change, list(value))
  }
}

#' Set/Get BRFSS Parameter Patterns
#'
#' Set a BRFSS parameter to be used in pattern matching . Examples of this are year,
#' geography, and version. Arguments are symbols so the syntax is
#' brfss.param(year = ") to set and brfss.param(year) to get/fetch.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
brfss.param_pattern <- function(...) {

  # get the arguments as unquoted

  arguments <- as.list(match.call())

  # first element is the function name itself ... if no params passed, exit
  if(length(arguments)==1) return(NULL)

  # the argument of interest is the second one

  arg <- arguments[2]
  my_brfss <- my.brfss.env()

  # if the argument is named, it is an assignment

  if(is.null(names(arg))) {

    # it is a retrieval ... get the value of the parameter

    param <-as.character(arg[[1]])
    return(my_brfss[[param]]$pattern)

  } else {

    #   it is an assignment ... assign the value to the parameter

    param <- names(arg)
    value <- arg[[1]]
    my_brfss[[param]]$pattern <- value
    my.brfss.save(my_brfss)


  }

  my_brfss
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
brfss.param_pats <- function(...) {

  arguments <- as.list(match.call())


  args <- arguments[-1]


  my_brfss <- my.brfss.env()

  if(length(args)>0) {
    mapply(function(arg, param) {

      my_brfss[[param]][2]<<-arg[[1]]

    }, args, names(args))

    my.brfss.save(my_brfss)
  } else {

    x <- sapply(my_brfss, function(myb) {

      if(length(myb)<2) return(NULL)

      myb[[2]]

    })
    return(x)
  }

}


#' Get or Set BRFSS Parameters
#'
#' @param ... Gets all parameters if no arguments, else sets the values of any parameters
#' passed as arguments in the form parameter = value
#'
#' @param val_only  logical - Whether to return only the value element of each parameter
#'
#' @return a list of parameters if no arguments are passed
#' @export
#'
#' @examples
#' # initialize the BRFSS parameters with default values and
#' #   change the geography of interest to Montana and the data file
#' #   source to ascii
#'\dontrun{
#' my.brfss.init()
#' brfss.params(geog = "MT", source = "ascii")
#'}
brfss.params <- function(... , val_only = TRUE) {

  args <- list(...)

  # get current environment/list

  my_brfss <- my.brfss.env()

  # if has args then save those args to the brfss env

  if(length(args) > 0) {

    # for each arg save [arg name] as [arg value]
    mapply( function(arg, nm) {
      set_param(nm, arg)

      #my_brfss[[nm]]$value<<-arg
    },args,names(args))
    my_brfss <- my.brfss.env()

  } else {
    # return the (possibly updated) list of params

    if(val_only) {
      my_brfss <- sapply(my_brfss, function(myb,nm) {
        myb$value
      })
    }

    return(my_brfss)
  }
}


my.brfss.save <- function(my_brfss) {
  path <- my.brfss.path()

  if(file.exists(path)) {
    saveRDS(my_brfss,file = path)
    return(TRUE)
  } else NULL

}

my.brfss.env <- function() {
  path <- my.brfss.path()

  if(file.exists(path)) {
    readRDS(file = path)
  } else NULL

}

default.brfss.env <- function() {

  extent <- my.brfss.default.extent()
  source <- my.brfss.default.source()
  geogs <- my.brfss.default.other.geogs()
  geog <- my.brfss.default.geog()
  year <- my.brfss.default.year()

  list(year=year, geog = geog, other_geogs = other_geogs, source = source, extent = extent)

}


my.brfss.path <- function() {
  folder <- orrr::convert.dot(apply.pattern("brfss_data_folder"))
  paste0(folder, "my_brfss.rds")
}


#' Initialize BRFSS Parameters
#'
#' Sets the BRFSS parameters to their default values.
#'
#' @details
#' The initial parameters are:
#'
#'  \itemize{
#'  \item{year}{ - the year of interest}
#'  \item{geog}{ - the geography of interest}
#'  \item{version}{ - the version of interest}
#'  \item{extent}{ - the extent of the raw data file ... local (one geog) or
#'  national (all geographies)}
#'  \item{source}{ - the file type source of the raw data ... ascii or sas}
#'}
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' my.brfss.init()
#' brfss.param(year)
#'}
my.brfss.init <- function() {
  path <- my.brfss.path()

  my_brfss <- list()

  my_brfss$year$value <- lubridate::year(Sys.Date()) - 1
  my_brfss$year$pattern <- "YEAR"

  my_brfss$year$on_change <- function(year) {

    if(!is.integer(year)) {

      # turn year into an integer and resave

      if(!orrr::is.integer_like(year)) {
        year <- lubridate::year(Sys.Date()) - 1
      } else {
        year <- as.integer(year)
      }
      brfss.param(year = year, myself = TRUE)

    }
    yr_val <- year%%100
    brfss.param(yr = yr_val)
  }

  my_brfss$year$on_null <- function(year) {
    year_val = lubridate::year(Sys.Date()) - 1
    brfss.param(year = year_val)
  }

  my_brfss$yr$value <- my_brfss$year$value%%100
  my_brfss$yr$pattern <- "YR"

  my_brfss$geog$value <- ""
  my_brfss$geog$pattern <- "GEOG"

  my_brfss$geog$on_change <- function(geog) {
    if(orrr::is.numeric_like(geog)) geog<-geog_abbs(geog)
    brfss.param(geog = geog, myself=TRUE)
  }


  my_brfss$geogs_other$value <- ""
  my_brfss$geogs_other$pattern <- ""

  my_brfss$source$value <- "ascii"
  my_brfss$source$pattern <- "SRC"
  my_brfss$source$values <- c("ascii","sas")

  my_brfss$extent$value <- "local"
  my_brfss$extent$pattern <- "EXT"
  my_brfss$extent$values <- c("local","national")

  my_brfss$extent$on_change <- function(extent) {
    if(extent == "local" && brfss.param(geog) == "") {
      warning("extent is local and geog cannot be blank")
    }
  }


  my_brfss$version$value <- 0
  my_brfss$version$pattern <- "VERS"

  saveRDS(my_brfss, file = path)

}

#' Get BRFSS Parameter Pattern List
#'
#' Retrieves the list of current pattern-value pairs for processing patterns for default
#' file and folder locations, as well as a few other items.
#'
#' @return list containing the pairs
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' pats <- my.brfss.patterns()
#'
#' df_brfss <- apply.pattern
#'
#' }
my.brfss.patterns <- function() {
  my_brfss <- brfss.params(val_only = FALSE)

  pats <- sapply(my_brfss, function(myb) {
    x <- as.vector(myb$value)
    pat <- as.character(myb$pattern)
    if(length(x)>0 && nchar(pat)>0) {
      names(x) <- unname(pat)
    } else x <- NA
    x
  })

  pats <- pats[!is.na(pats)]

  names(pats) <- gsub("(.*)[.]","",names(pats))
  pats
}
