#https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018ASC.zip

#' URL of BRFSS ASCII data
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
ascii_data_url<-function(year) {
  paste0("https://www.cdc.gov/brfss/annual_data/", year,"/files/LLCP",year,"ASC.zip")
}
#

ascii.process.year <- function(year = NULL, codebook = TRUE, download = TRUE, convert = TRUE) {

  year <- get.year(year)

  if(codebook) {
    download.codebook(year = year)
    save_codebook_layout(year = year)
  }

  if(download) {
    if(verbose) cat(" ... downloading ... main ascii file ... ")
    ascii.download.data(year=year)
  }

  if(convert) {
    convert.ascii(year = year)
  }
}


#' Download ASCII BRFSS data
#'
#' Download the annual ASCII BRFSS data files, all versions
#'
#'
#' @param year integer - year of data file
#' @param destpath character - location to store file - will be created if it does not exist - default is the data_raw folder in the project root folder
#' @param unzip logical - unzip file?
#' @param rmzip logical - remove the zip file?
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ascii.download.data(2016)
#' }
#'
#'
ascii.download.data<-function(year = NULL, destpath = NULL, unzip=TRUE, rmzip=TRUE) {

  year <- get.year(year)

  if(is.null(destpath)) destpath <- apply.pattern("ascii_raw_data_folder", YEAR = year)
  destpath <- normalizePath(destpath,winslash = "/",mustWork = FALSE)
  #
  if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)

  version <- 0

  cont <- TRUE
  ctr <- 0
  while(cont && ctr<6) {
    ctr <- ctr + 1

    url<-apply.pattern("ascii_downloads_url",YEAR =year, VERS =version)
    destfile<-apply.pattern("ascii_path_zip",YEAR =year, VERS =version)


    status <- httr::HEAD(url)$status
    if(status==200) {

      path <- dirname(destfile)

      if(!dir.exists(path)) dir.create(path,recursive = TRUE)

      ## the larger BRFSS files began to take more than 60 seconds to download
      ##  so a larger timeout option is necessary

      to <- getOption("timeout")
      options(timeout = 180)

      download.file(url = url,destfile = destfile,
                    method = "libcurl",quiet = F, mode = "wb")

      options(timeout=to)

      if(unzip) {
        exdir<-gsub("/$","", path)
        unzip(destfile,exdir = exdir)
        if(rmzip) file.remove(destfile)
      }

    } else {

      cont <- FALSE
    }
    version <- version + 1
  }

}

#' Convert BRFSS raw ASCII data files
#'
#'  Convert BRFSS raw ASCII data files to data frames and save
#'
#' @param year integer year of interest
#' @param layout character name of the file containing the layout or data.frame containing the layout
#' @param completes logical whether or not to include only complete interviews (default = TRUE)
#' @param main logical include main ascii file (version 0) (default = TRUE)
#' @param versions logical include ascii file versions (default = TRUE)
#' @param verbose logical print some extra information during processing (default = FALSE)
#'
#' @return data frame containing survey data
#'
#' @examples
#' \dontrun{
#' my.brfss(year = 2020, geog = "MT")
#' df<-read.brfss.ascii(year = ,
#' layout="./data_raw/var_layouts/layout_2016")
#'}
#'
#' @export

convert.ascii<-function(year=NULL,layout = NULL, completes=T, main = TRUE, versions = TRUE, verbose = FALSE) {
  browser()
  year<-get.year(year)

  if(is.null(layout)) {
    layout <- get.codebook.layout(year)

  }

  if(is.null(layout)) {
    return (NA)

  }

  if(main) version <- 0 else {
    if(versions) version = 1 else return()
  }
  path <- apply.pattern("ascii_path_raw", YEAR = year, VERS = version)

  while (file.exists(path)) {
    if(verbose) cat("... reading version [", version, "] : ", path, "\n")
    df <- read.ascii(filename = path, layout = layout, verbose = verbose)

    df_name <- paste0("df_ascii_",year)
    if(version>1) df_name <- paste0(df_name,"_V",version)
    assign(df_name,value = df)
    path <- apply.pattern("ascii_path", YEAR = year, VERS = version)
    if(!dir.exists(dirname(path))) dir.create(dirname(path))

    if(verbose) cat("... writing ", df_name, "to ", path,"\n")

    save(list = c(df_name), file = path)

    version <- version + 1
    path <- apply.pattern("ascii_path_raw", YEAR = year, VERS = version)

  }

  invisible()
}

#' Convert a BRFSS raw ASCII data file
#'
#'  Read BRFSS raw ASCII data file and save as data frame in .RData file
#'
#' @param filename character name of the raw ascii data file
#' @param layout character name of the file containing the layout or data.frame containing the layout
#' @param completes logical whether or not to include only complete interviews (default=TRUE)
#' @param verbose logical print some extra information during processing (default = FALSE)
#'
#' @return data frame containing survey data
#'
#' @examples

#'
#' @export

read.ascii<-function(filename=NULL,layout = NULL, completes=T, verbose = FALSE) {

  #  env<-get.brffs.env()
  # state <- match.arg(state)


  if(is.null(filename) || is.null(layout)) {
    return (NA)

  }


  ################################
  ##
  ##  get the layout data
  ##
  if(class(layout)=="character") {
    df_fields_yy<-read.brfss.layout(layout)
  } else {
    df_fields_yy<-layout
  }

  ############################################################
  ##
  ##  remove col_names of negative width columns
  ##
  widths = as.integer(df_fields_yy$field_size)

  col.names = df_fields_yy$col_name[widths>0]

  ################################
  ##
  ##  read the file based on the layout
  ##

  df_all <- data.frame()

  buffersize = 100000

  cont <- TRUE

  n <- 100000
  #nlines <- length(readLines(filename))
  skip <- 0
  i <- 0

  time0 <- last_time <- Sys.time()

  if(verbose) cat(format(time0, "%a %b %d %Y %X"), "... Starting \n")

  while (cont) {
    x <- system.time(
      df<-read.fwf(filename,widths = widths,
                   col.names = col.names,
                   skip = skip,
                   n = n,
                   buffersize = buffersize,
                   check.names = FALSE
      )

    )

    df_times <<- dplyr::bind_rows(df_times,
                                  data.frame(sysTime = Sys.time(),
                                             seconds = x["elapsed"],
                                             skip,n0 = n,nact = nrow(df), buffersize))
    df_all <<- rbind(df_all,df)


    skip <- skip + n

    cont <- (nrow(df) == n)

    i <- i + 1
    tim <- Sys.time()
    mins0 <- lubridate::as.period(tim - time0)
    mins1 <- lubridate::as.period(last_time - time0)

    mins0 <- mins0@minute + round(mins0@.Data/60,1)
    mins1 <- mins1@minute + round(mins1@.Data/60,1)

    if(verbose) cat("[",i, "] ",
                    format(Sys.time(), "%a %b %d %Y %X"),
                    "... just read: ",  nrow(df), " lines (",mins1," min) ",
                    "... so far: ",  nrow(df_all), "lines (",mins0," min)\n")

    last_time <- tim
  }

  ######################################
  ##
  ##  use the value of state to determine which rows to keep
  ##


  #######################################################
  ##
  ##  if completes only then make sure DISPCODE in completes range (<2000)
  ##
  if (completes) yn <- (df$DISPCODE<2000)

  ##########################################################
  ##
  ##  return the data frame where conditions (yn) are true
  ##
  df<- df[yn,]


}

