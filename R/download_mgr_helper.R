
download_sas_data<-function(file_mgr = NULL, folderout = NULL, year = NULL) {


  #files<-sas.url.pattern.downloads.versions()
  file_pttrn <- file_mgr$get("xpt_download_zip_file")

  if(!is.null(year)) file_mgr$dataset_mgr$set(year = year)

  if(is.null(folderout)) folderout <- file_mgr$apply("sas_raw_data_folder")

  folderout <- gsub("([^[/])$","\\1/",folderout)

  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles <- file_mgr$apply("brfss_url_files")


  to <- getOption("timeout")
  options(timeout = 180)

  ok <- TRUE
  version <- 0

  while(ok) {

    file_mgr$dataset_mgr$set(version = version)

    file <- file_mgr$apply("xpt_download_zip_file")

    url<-paste0(urlfiles,file)
    status <- httr::HEAD(url)$status
    if(status==200) {

      fileout<-paste0(folderout,file)

      cat(paste0("XPT files ... downloading ... ", url), "\n")

      download.file(url = url,destfile = fileout,
                    method = "libcurl", quiet = TRUE)

      if(grepl("[.]zip$",fileout)) {
        unzip(fileout,exdir = normalizePath(folderout))
        file.remove(fileout)
      } # end if(grepl()


    } else {
      ok <- FALSE
    }
    version <- version + 1
  }
  options(timeout = to)
  file_mgr$dataset_mgr$set(version = 0)

  invisible()
}


download_ascii_data <-function(file_mgr = NULL, destpath = NULL, unzip=TRUE, rmzip=TRUE) {


  if(is.null(destpath)) destpath <- file_mgr$apply("ascii_raw_data_folder")
  destpath <- normalizePath(destpath,winslash = "/",mustWork = FALSE)
  #

  if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)

  version <- 0

  cont <- TRUE
  ctr <- 0

  while(cont && ctr<6) {

    file_mgr$dataset_mgr$set(version = version)

    ctr <- ctr + 1

    url<-file_mgr$apply("ascii_downloads_url")
    destfile<-file_mgr$apply("ascii_zip_path")

    cat(paste0("Downloading ... trying ", url), "\n")

    status <- httr::HEAD(url)$status
    if(status==200) {

      path <- dirname(destfile)

      if(!dir.exists(path)) dir.create(path,recursive = TRUE)

      ## the larger BRFSS files began to take more than 60 seconds to download
      ##  so a larger timeout option is necessary

      to <- getOption("timeout")
      options(timeout = 180)

      cat(stringr::str_wrap(paste0("Downloading ... trying ", url),50), "\n")

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

  file_mgr$dataset_mgr$set(version = 0)


}

download_metadata = function(file_mgr = NULL, folderout = NULL, year = NULL) {

  pttrns <- file_mgr$pattern_group("sas_downloads") %>% pull(pattern)
  urlfiles<- file_mgr$apply("brfss_url_files")

  #  folderout<-apply.pattern("sas_raw_data_folder", params)

  if(!is.null(year)) file_mgr$dataset_mgr$set(year = year)

  if(is.null(folderout)) folderout <- file_mgr$apply("brfss_annual_raw_metadata_folder")

  folderout <- gsub("([^[/])$","\\1/",folderout)

  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  to <- getOption("timeout")
  options(timeout = 300)


  sapply(pttrns,function(pttrn) {

    has_vers <- grepl("^VERS^", pttrn, fixed = TRUE)
    cont <- TRUE
    version <- 0
    file_mgr$dataset_mgr$set(version = version)

    while(cont) {

      browser()

      filename <- file_mgr$patternize(strIn = pttrn)
      url<-paste0(urlfiles,filename)
      fileout<-paste0(folderout,filename)

      #
      # download will fail on larger files if time to download is > 60 secs
      #   set time to 3 minutes and then restore when done

      x <- httr::GET(url = url)

      if(x$status_code  > 399) {
        url <- gsub("SAS$", "zip", url)
        x <- httr::GET(url = url)

      }

      cont <- x$status_code == 200
      if(cont) {
        cat(paste0("Metadata ... downloading ", filename), "\n")

        download.file(url = url,destfile = fileout,
                      method = "libcurl",quiet = TRUE)

        if(grepl("[.]zip$",fileout)) {
          unzip(fileout,exdir = normalizePath(folderout))
          file.remove(fileout)
        }

        version <- version + 1
        file_mgr$dataset_mgr$set(version = version)

      }
      cont <- has_vers && cont
    }
  })

  options(timeout = to)
  file_mgr$dataset_mgr$set(version = 0)

  return(invisible(NULL))

}

