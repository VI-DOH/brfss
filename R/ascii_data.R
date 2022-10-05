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

#' Process BRFSS Annual Data Files
#' Use this function to process a single year of BRFSS data. This function sill download, unzip,
#' and create a data frame with all data from the ASCII .DAT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param download - logical - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param convert - logical - read the xpt files into data_frames and save? Useful (set = FALSE)
#' if you already have the xpt files processed
#' @param saq - logical - are there state-added questions for processing
#' @param codebook - logical - download and process the annual codebook
#' @param split - logical - split the xpt file by state/geography
#' @param geog - character - geography of interest
#' @param factorize - logical - add values as factors to columns
#' @param ... further arguments passed to other methods
#'
#' @return invisible()
#' @export
#'
#' @examples
#' \dontrun{
#' ascii_process_year(year = 2020, download=TRUE, layout = TRUE, codebook = TRUE, convert = TRUE, split = TRUE)
#'
#' }

ascii_process_year <- function(dl_metadata = FALSE, dl_codebook = FALSE,
                               dl_data = FALSE, codebook = TRUE, attribs = TRUE,
                               saq = FALSE, layout = FALSE,
                               convert = TRUE, split = TRUE, factorize = TRUE,
                               responses = TRUE, verbose=FALSE, progress = NULL) {

  use_progress <- !is.null(progress)

  if(dl_metadata) {
    if(verbose) cat(" ... downloading ... metadata ... ")
    sas_download_metadata(progress = progress)
  }

  if(dl_codebook) {
    if(verbose) cat(" ... downloading ... codebook ... ")

    download_codebook(progress = progress)
  }

  if(dl_data) {
    if(verbose) cat(" ... downloading ... ascii data ... ")
    ascii.download.data(progress = progress)
  }

  if (layout) save_sas_layout(progress = progress)

  if(codebook) {
    process_codebook(progress = progress)
  }

  if(saq) {

    build_saq_layout()
    merge_saq_layout()

    build_saq_values()
    merge_saq_values()


  }

  if(convert) {
    convert_ascii(verbose=verbose, progress = progress)
  }

  if(attribs) add_column_attributes()

  if(split) split_geogs(progress = progress)

  if(factorize) factorize( progress = progress)

  if(responses) {
    save_response_stats()
    save_module_stats()
  }
  invisible()

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
ascii.download.data<-function(destpath = NULL, unzip=TRUE, rmzip=TRUE, progress = NULL) {

  params <- my.brfss.patterns()

  show_progress(progress, message = "Downloading ... ")


  if(is.null(destpath)) destpath <- apply.pattern("ascii_raw_data_folder",params)
  destpath <- normalizePath(destpath,winslash = "/",mustWork = FALSE)
  #
  if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)

  version <- 0

  cont <- TRUE
  ctr <- 0
  while(cont && ctr<6) {
    brfss.param(version = version)
    ctr <- ctr + 1
    params <- my.brfss.patterns()

    url<-apply.pattern("ascii_downloads_url",params)
    destfile<-apply.pattern("ascii_zip_path",params)

    show_progress(progress,
                  message = paste0("Downloading ... trying ", url))

    status <- httr::HEAD(url)$status
    if(status==200) {

      path <- dirname(destfile)

      if(!dir.exists(path)) dir.create(path,recursive = TRUE)

      ## the larger BRFSS files began to take more than 60 seconds to download
      ##  so a larger timeout option is necessary

      to <- getOption("timeout")
      options(timeout = 180)

      show_progress(progress,
                    message = stringr::str_wrap(paste0("Downloading ... trying ", url),50))

      download.file(url = url,destfile = destfile,
                    method = "libcurl",quiet = TRUE, mode = "wb")

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
convert_ascii<-function(layout = NULL, completes=T, main = TRUE,
                        versions = TRUE, verbose = FALSE, progress = NULL) {

  show_progress(progress, message = "Converting ... ")

  params <- my.brfss.patterns()

  if(is.null(layout)) {
    layout <- get.layout()
  }

  if(is.null(layout)) {
    return (NULL)
  }

  if(main) version <- 0 else {
    if(versions) version = 1 else return()
  }

  file_raw <- apply.pattern("ascii_filename_raw", params)
  path_raw <- apply.pattern("ascii_path_raw", params)

  while (file.exists(path_raw)) {

    if(verbose) cat("... reading version [", version, "] : ", path_raw, "\n")

    show_progress(progress, message =
                    paste0("Converting ... version [", version, "] : ", file_raw))

    df <- read.ascii(filename = path_raw, layout = layout, verbose = verbose)

#    df <- add_col_attributes(df)

    path <- apply.pattern("brfss_annual_data_path",params)
    if(!dir.exists(dirname(path))) dir.create(dirname(path))

    if(verbose) cat("... writing ", path,"\n")

    show_progress(progress, message =
                    paste0("Converting ... saving version [", version, "] ", path))

    saveRDS(df, file = path)

    version <- version + 1
    brfss.param(version = version)
    params <- my.brfss.patterns()
    path_raw <- apply.pattern("ascii_path_raw", params)

  }

  brfss.param(version = 0)

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

  col_types <- layout$var_type
  names(col_types) <- layout$col_name
  col_types[grepl("^DUMMY",names(col_types))] <- 'NULL'

  ## this is a kludge ... the weight fields should be numeric

  col_types[grepl("_.*WT.*",names(col_types))] <- 'numeric'

  # end kludge

  widths <- layout$field_size

  x<-readLines(filename)


  df <- iotools::dstrfw(x = x,col_types = col_types, widths = widths)



  #######################################################
  ##
  ##  if completes only then make sure DISPCODE in completes range (<2000)
  ##
  if (completes) df <- df %>% filter(DISPCODE<2000)

  ##########################################################
  ##
  ##  return the data frame
  ##

  df
}

#' Split BRFSS ASCII data by Geography
#'
#' The main BRFSS ASCII data file created when the XPT file is downloaded and read has data
#' for all geographies. This function splits out the geographies of interest.
#'
#' @param year integer - year of interest
#' @param main logical - process main ascii data file
#' @param versions logical - process versioned ascii file
#' @param my_geog character - abbreviation for primary state/geography of interest (e. "MT")
#' @param other_geogs character - abbreviations for other states/geographies of interest (e. c("ID","WY"))
#' @param verbose logical - provide details during processing
#'
#' @export
#' @examples
#'
#'\dontrun{
#' cleave.geogs.ascii(year = 2020, main=TRUE,versions=TRUE,
#' my_geog="MT", other_geogs=NULL,verbose=TRUE)
#'}
#'
cleave.geogs.ascii<-function(year = NULL,
                             main=TRUE,versions=TRUE, my_geog=NULL, other_geogs=NULL,verbose=TRUE) {

  if(!(main || versions)) return(NULL)

  year <- get.year(year)

  if(is.null(my_geog)) my_geog <- my.geog()
  if(is.null(other_geogs)) other_geogs <- my.other.geogs()

  if(my_geog=="") my_geog <- character(0)

  geogs <- get.geogs()

  ver<-integer(0)
  if(main) ver<-0

  vermax <- highest_version()

  if(versions) ver<-c(ver,1:vermax)

  df_geogs <- get_geogs_all()

  sapply(ver,function(version) {

    rdata_file <- apply.pattern("ascii_path",YEAR = year, VERS = version)

    df_xpt<-load.sas(year,rdata_file = rdata_file, version)

    if(geogs[1] == '*') {
      geogs<-unique(df_xpt$`_STATE`)

    } else {

      if(is.character(geogs)) {
        geogs<-sapply(geogs,function(state) {
          df_geogs[df_geogs$Abbrev==state,"Id"]
        })

        geogs <- unlist(unname(geogs))
      }
    }

    add_cols<-character(0)

    mapply(function(id,nm) {

      if(id%in%geogs) {

        df_state<-df_xpt[df_xpt$`_STATE`==id,]

        if(nrow(df_state)>0) {
          if(verbose) cat("Saving ",nm,"V",version,"\n")
          sapply(1:ncol(df_xpt),function(i) {
            attrs<-attributes(df_xpt[[i]])

            if(!is.null(attrs)){
              sapply(1:length(attrs),function(j) {
                #browser()
                attr(df_state[[i]],names(attrs[j]))<<-attr(df_xpt[[i]],names(attrs[j]))
              })
            } else {
              add_cols<<-c(add_cols,colnames(df_xpt)[i])
            }

          })

          dfname<-paste0("df_",nm,"_",year)
          if(version>0) dfname<-gsub(nm,paste0(nm,"_V",version),dfname)

          assign(dfname,df_state)

          fname <- brfss_data_path(year = year, geog = nm, version = version, rw = 'w')

          if(verbose) cat("Going to save :", fname, "\n")

          saveRDS(df_state,file = fname)

          #columns.add(year,add_cols)
        }
      }
    },df_geogs$Id,df_geogs$Abbrev)
  })
  invisible()
}

# read.ascii<-function(filename=NULL,layout = NULL, completes=T, verbose = FALSE) {
#
#   #  env<-get.brffs.env()
#   # state <- match.arg(state)
#
#
#   if(is.null(filename) || is.null(layout)) {
#     return (NA)
#
#   }
#
#
#   ################################
#   ##
#   ##  get the layout data
#   ##
#   if(class(layout)=="character") {
#     df_fields_yy<-read.brfss.layout(layout)
#   } else {
#     df_fields_yy<-layout
#   }
#
#   ############################################################
#   ##
#   ##  remove col_names of negative width columns
#   ##
#   widths = as.integer(df_fields_yy$field_size)
#
#   col.names = df_fields_yy$col_name[widths>0]
#
#   ################################
#   ##
#   ##  read the file based on the layout
#   ##
#
#   df_all <- data.frame()
#
#   buffersize = 100000
#
#   cont <- TRUE
#
#   n <- 100000
#   #nlines <- length(readLines(filename))
#   skip <- 0
#   i <- 0
#
#   time0 <- last_time <- Sys.time()
#
#   if(verbose) cat(format(time0, "%a %b %d %Y %X"), "... Starting \n")
#
#   while (cont) {
#     x <- system.time(
#       df<-read.fwf(filename,widths = widths,
#                    col.names = col.names,
#                    skip = skip,
#                    n = n,
#                    buffersize = buffersize,
#                    check.names = FALSE
#       )
#
#     )
#
#     df_times <- dplyr::bind_rows(df_times,
#                                  data.frame(sysTime = Sys.time(),
#                                             seconds = x["elapsed"],
#                                             skip,n0 = n,nact = nrow(df), buffersize))
#     df_all <- rbind(df_all,df)
#
#
#     skip <- skip + n
#
#     cont <- (nrow(df) == n)
#
#     i <- i + 1
#     tim <- Sys.time()
#     mins0 <- lubridate::as.period(tim - time0)
#     mins1 <- lubridate::as.period(tim - last_time)
#
#     mins0 <- mins0@minute + round(mins0@.Data/60,1)
#     mins1 <- mins1@minute + round(mins1@.Data/60,1)
#
#     if(verbose) cat("[",i, "] ",
#                     format(Sys.time(), "%a %b %d %Y %X"),
#                     "... just read: ",  nrow(df), " lines (",mins1," min) ",
#                     "... so far: ",  nrow(df_all), "lines (",mins0," min)\n")
#
#
#     last_time <- tim
#   }
#
#   #######################################################
#   ##
#   ##  if completes only then make sure DISPCODE in completes range (<2000)
#   ##
#   if (completes) df_all <- df_all %>% filter(DISPCODE<2000)
#
#   ##########################################################
#   ##
#   ##  return the data frame where conditions (yn) are true
#   ##
#   df_all
#
#
# }

