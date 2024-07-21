

# brfss.url.pattern.files<-function() {
#   "https://www.cdc.gov/brfss/annual_data/[YEAR]/files/"
# }
# # https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip

brfss.url.pattern.documentation<-function() {
  "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/"
}

#https://www.cdc.gov/brfss/annual_data/2016/pdf/2016_calculated_variables_version4.pdf

sas.url.pattern.download.doc<-function() {
  c(
    "codebook[YR]_llcp.pdf"
  )
}

unzip.all<-function(rmzip=TRUE) {

  params <- my.brfss.patterns()

  folder<-apply.pattern("sas_raw_data_folder", params)
  files<-list.files(folder,full.names = T)

  files<-files[grep("[.]zip$",files)]
  folder<-normalizePath(folder,winslash = "/")
  folder<-gsub("[/]$","",folder)
  invisible(
    sapply(files, function(file) {
      #browser()
      unzip(zipfile = file, exdir = folder,overwrite = T)
      if(rmzip) file.remove(file)
    })
  )


}

missing.columns<-function(year) {
  e<-new.env()
  fname<-paste0("./data/",year,"/columns_",year,".RData")
  load(file = fname,envir = e)
  df<-get(ls(e),envir = e)
  df[is.na(df$label),"varname"]

}

has.columns<-function(year,cols,nolabel=F) {
  e<-new.env()
  fname<-paste0("./data/",year,"/columns_",year,".RData")
  load(file = fname,envir = e)
  df<-get(ls(e),envir = e)
  if(!nolabel) df<-df[!is.na(df$label),]
  cols%in%df$varname

}


fix.missing.columns<-function(year,year2=year-1) {
  e<-new.env()
  fname<-paste0("./data/",year,"/columns_",year,".RData")
  load(file = fname,envir = e)
  df<-get(ls(e),envir = e)
  df_fix<-df[is.na(df$label),]
  df<-df[!is.na(df$label),]

  e2<-new.env()
  fname1<-paste0("./data/",year2,"/columns_",year2,".RData")
  load(file = fname1,envir = e2)
  df1<-get(ls(e2),envir = e2)

  df1<-df1[df1$varname%in%df_fix$varname,]
  df_fix<-df_fix[!df_fix$varname%in%df1$varname,]
  df<-rbind(df,df_fix,df1)

  assign(ls(e),df)
  saveRDS(df,file =fname )

}

#' Process BRFSS Annual Data Files
#' Use this function to process a single year of BRFSS data. This function sill download, unzip,
#' and create a data frame with all data from the XPT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param download - logical - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param layout - logical - save sas layout
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
#' sas_process_year(dl_metadata=TRUE, dl_codebook = TRUE, dl_data = TRUE)
#'
#' }

sas_process_year <- function(dl_metadata = FALSE, dl_codebook = FALSE,
                             dl_data = FALSE, layout = TRUE,
                             codebook = TRUE, convert = TRUE, attribs = TRUE, saq = FALSE,
                             split = TRUE, responses = TRUE,
                             factorize = TRUE, verbose=FALSE, progress = NULL,
                             ...)  {



  if(dl_metadata) {
    if(verbose) cat(" ... downloading ... metadata ... ")
    sas_download_metadata(progress = progress)
  }

  if(dl_codebook) {
    if(verbose) cat(" ... downloading ... codebook ... ")

    download_codebook(progress = progress)
  }

  if(dl_data) {
    if(verbose) cat(" ... downloading ... metadata ... ")

    sas_download_xpt(progress = progress)
  }

  # if(verbose) cat(" versions ")
  # sas_download_metadata.versions(year=year)
  if(verbose) cat(" \n ... unzipping files\n ")
  unzip.all()

  if (layout) save_sas_layout(progress = progress)

  if(codebook) process_codebook(progress = progress)

  if(saq) {
    build_saq_layout()
    merge_saq_layout()

    build_saq_values()
    merge_saq_values()
  }

  if(convert) {
    if(verbose) cat(" ... reading main xpt file\n ")
    convert_sas(verbose = verbose, progress = progress)


  }

  if(attribs) add_column_attributes()

  if(split) split_geogs( factorize = factorize)

  if(factorize) factorize( progress = progress)

  if(responses) {
    save_response_stats()
    save_module_stats()
  }

  invisible()
}

#' Convert SAS
#'
#' @param verbose
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
convert_sas <- function(verbose = FALSE, progress = NULL) {

  show_progress(progress,
                message = paste0("Survey Data ... converting .xpt main file"))

  read.xpt(version = 0)

  cont <- TRUE

  ivers<-1
  if(verbose) cat(" ... trying versions\n ")
  while (cont) {

    if(verbose) cat("Converting version=",ivers,"\n")
    show_progress(progress,
                  message = paste0("Survey Data ... converting version ", ivers))

    cont <- read.xpt(version=ivers,verbose=TRUE)


    ivers<-ivers + 1
  }
}

sas_download_metadata<-function(year, progress = NULL, ...) {
  #  files<-sas.url.pattern.downloads.data()
  params <- my.brfss.patterns()

  show_progress(progress,
                message = "Metadata ... downloading")

  pttrns<-get.pattern.group("sas_downloads")
  urlfiles<- apply.pattern("brfss_url_files", params)

#  folderout<-apply.pattern("sas_raw_data_folder", params)

  folderout<-apply.pattern("brfss_annual_raw_metadata_folder", params)


  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  to <- getOption("timeout")
  options(timeout = 180)


  sapply(pttrns,function(pttrn) {

    has_vers <- grepl("^VERS^", pttrn, fixed = TRUE)
    cont <- TRUE
    version <- 0
    params["VERS"] <- 0

    while(cont) {
      filename<-patternize(strIn = pttrn, params)
      url<-paste0(urlfiles,filename)
      fileout<-paste0(folderout,filename)

      #
      # download will fail on lager files if time to download is > 60 secs
      #   set time to 3 minutes and then restore when done

      x <- httr::GET(url = url)
      cont <- x$status_code == 200
      if(cont) {
        show_progress(progress,
                      message = paste0("Metadata ... downloading ", filename))

        download.file(url = url,destfile = fileout,
                      method = "libcurl",quiet = !is.null(progress))

        if(grepl("[.]zip$",fileout)) {
          unzip(fileout,exdir = normalizePath(folderout))
          file.remove(fileout)
        }

        version <- version + 1
        params["VERS"] <- version
      }
      cont <- has_vers && cont
    }
  })

  options(timeout = to)

}

#' Download SAS XPT files from CDC website
#'
#' @param year integer - year of interest
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @export
#'
sas_download_xpt<-function(progress = NULL) {


  show_progress(progress,
                message = "XPT files ... downloading ... ")

  params <- my.brfss.patterns()
  #files<-sas.url.pattern.downloads.versions()
  file_pttrn<-get.pattern("xpt_download_zip_file")

  folderout<-apply.pattern("sas_raw_data_folder", params)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles<-apply.pattern("brfss_url_files", params)


  to <- getOption("timeout")
  options(timeout = 180)

  ok <- TRUE
  version <- 0

  while(ok) {

    brfss.param(version = version)
    params <- my.brfss.patterns()

    file<-patternize(strIn = file_pttrn, params)

    url<-paste0(urlfiles,file)
    status <- httr::HEAD(url)$status
    if(status==200) {

      fileout<-paste0(folderout,file)

      show_progress(progress,
                    message = paste0("XPT files ... downloading ... ", url))

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
  invisible()
}


sas_download_metadata.versions<-function() {

  params <- my.brfss.patterns()

  #files<-sas.url.pattern.downloads.versions()
  files<-get.pattern.group("sas_version_downloads")

  folderout<-apply.pattern("brfss_annual_raw_metadata_folder",params)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles<-apply.pattern("brfss_url_files",params)

  sapply(files,function(file) {
    done <- FALSE
    sapply(1:6,function(version) {

      brfss.param(version = version)
      params <- my.brfss.patterns()

      file<-patternize(strIn = file,params )
      url<-paste0(urlfiles,file)
      if(!done) { #RCurl::url.exists(url)){
        fileout<-paste0(folderout,file)

        oldw <- getOption("warn")
        options(warn = -1)
        #      for (file in files) {

        tryCatch(expr = {

          to <- getOption("timeout")
          options(timeout = 180)

          download.file(url = url,destfile = fileout,
                        method = "libcurl")

          options(timeout = to)

          if(grepl("[.]zip$",fileout)) {
            unzip(fileout,exdir = normalizePath(folderout))
            file.remove(fileout)
          } # end if(grepl()

        }, # end expr =

        error = function(e) {
          print('Done downloading this pattern')
          done <<-TRUE
        }) # end tryCatch

        #}  # end for(file

        options(warn = oldw)      }
    })  # end sapply(1:
  })  # end sapply(files

  invisible()
}

#' Read BRFSS Data from SAS XPT File
#'
#' Creates a data frame from the xpt file provided by CDC. The sasout file is used to provide
#' some useful attributes for each column.
#' This function gets the default file names folder location from the naming_patterns data.
#'
#' @param version integer: version of interest (default is 0 ... main survey)
#' @param verbose logical: provide extra information during processing
#'
#' @return
#' @export
#'

#' @examples
#' \dontrun{
#' df_2021 <- read.xpt(2021)
#'
#' df_data <- read.xpt(2020, version= 1)
#' }

read.xpt<-function(version = 0, verbose = F) {


  brfss.param(version = version)
  params <- my.brfss.patterns()

  params["GFLAG"] <- "off"

  ########################################################################%%%%%%%%%
  ##
  ##    If file and folder names not supplied, create them from the file patterns

  ##
  ##    get sasout location
  ##
  sasout_file<-apply.pattern("sas_sasout_path",params)

  ##
  ##    get xpt raw data location
  ##

  xpt_file <- apply.pattern("xpt_path",params)


  ##
  ##  get save file (.rdata) location
  ##

  save_file<- apply.pattern("brfss_annual_data_path",params)

  ##
  ##    read the xpt files
  ##

  if(version>0) {
    if(verbose) cat("Trying version ",version,"\n")
  } else {
    if(verbose) cat("Trying main file \n")
  }

  if(file.exists(xpt_file)) {
    if(verbose) cat("Reading ",xpt_file,"\n")
    df_xpt<- haven::read_xpt(xpt_file)

    # st <- fips::state_fips(geog)
    # df_xpt <- df_xpt %>% filter(`_STATE` == st)

    cat("Getting sasout\n")


    #df_xpt <- df_xpt %>% add_col_attributes()

    if(!dir.exists(dirname(save_file))) dir.create(dirname(save_file),recursive = T)

    saveRDS(df_xpt,file = save_file)

    return(TRUE)
  } else {
    if(verbose) cat(xpt_file," doesn't exist\n")
    return(FALSE)
  }
}


#' Split BRFSS XPT by Geography
#'
#' The main BRFSS XPT data file created when the XPT file is downloaded and read has data
#' for all geographies.This function splits out the geographies of interest.
#'
#' @param year integer - year of interest
#' @param main logical - process main XPT file
#' @param versions logical - process versioned XPT file
#' @param my_geog character - abbreviation for primary state/geography of interest (e. "MT")
#' @param other_geogs character - abbreviations for other states/geographies of interest (e. c("ID","WY"))
#' @param verbose logical - provide details during processing
#'
#' @export
#' @examples
#'
#'\dontrun{
#' cleave.geogs.sas(year = 2020, main=TRUE,versions=TRUE, my_geog="MT", other_geogs=NULL,verbose=TRUE)
#'}
#'
cleave.geogs.sas<-function(year = NULL,
                           main=TRUE,versions=TRUE, my_geog=NULL,
                           other_geogs=NULL,verbose=TRUE) {

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

    rdata_file <- apply.pattern("sas_data_path",YEAR = year, VERS = version)

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

          # dfname<-paste0("df_",nm,"_",year)
          # if(version>0) dfname<-gsub(nm,paste0(nm,"_V",version),dfname)

          dfname<-apply.pattern("brfss_data_df", YEAR= year, GEOG = nm, VERS = version)

          assign(dfname,df_state)

          fname <- brfss_data_path( rw = 'w')

          #fname <- brfss_data_path(year = year, geog = nm, version = version, rw = 'w')

          if(verbose) cat("Going to save :", fname, "\n")

          saveRDS(df_state,file = fname)

          #columns.add(year,add_cols)
        }
      }
    },df_geogs$Id,df_geogs$Abbrev)
  })
  invisible()
}

columns.add<-function(year,cols2add){
  e<-new.env()

  fname<-apply.pattern("brfss_columns_path",YEAR = year)
  load(file = fname,envir = e)

  df<-get(ls(e),envir = e)

  cols2add<-cols2add[!cols2add%in%df$varname]
  if(length(cols2add)>0) {
    varcols<-colnames(df)
    df_new<-data.frame(matrix(nrow = length(cols2add),ncol = length(varcols)),stringsAsFactors = F )
    colnames(df_new)<-colnames(df)
    df_new$varname<-cols2add
    df<-rbind(df,df_new)
    assign(ls(e),df)

    saveRDS(df,file = fname )
  }
}

load.sas<-function(year, rdata_file=NULL, version=0) {

  ##
  ##  get save file (.rdata) location
  ##

  if(is.null(rdata_file)) {
    # if(version == 0) {
    rdata_file <- apply.pattern("sas_data_path",YEAR = year, VERS = version)
    # } else {
    #   rdata_file<- apply.pattern("sas_data_version",YEAR = year, VERS = version)
    # }
  }

  e<-new.env()

  load(file = rdata_file,envir = e)
  get(ls(e),envir = e)
}


read.sas.format<-function(folder_pat= NULL, file_pat = NULL) {
  require(dplyr)

  params <- my.brfss.patterns()

  if(is.null(folder_pat)) folder_pat <- get.pattern("sas_raw_data_folder") %>% expand.pattern()
  if(is.null(file_pat)) file_pat <- expand.pattern(get.pattern("sas_file_format"))


  folder<-patternize(folder_pat, params)
  file<-patternize(file_pat, params)
  paste0(folder,file)

  lines<-readLines(paste0(folder,file))
  lines<-lines[grep(";",lines,invert = T)]
  lines<-lines[grep("^ *[.]",lines,invert = T)]
  vlines <- grep("VALUE ",lines)
  values<-gsub(".*VALUE *([^ ]*) *","\\1",lines[vlines])
  nvals<-length(vlines)

  vstart<-vlines+1
  vend<-vlines[2:nvals]-1
  vend[nvals]<-length(lines)

  df<-data.frame(stringsAsFactors = F)

  sapply(1:length(values), function(i) {
    df2<-data.frame(stringsAsFactors = F)
    sapply(vstart[i]:vend[i],function(j) {
      line<-lines[j]
      value<-gsub("(.*?)=.*","\\1",line)
      value<-gsub(" ","",value)

      text <-gsub("(.*?)=(.*)","\\2",line)
      text<-gsub("^ *(.*) *$","\\1",text)
      text<-gsub("\"","",text)

      df2<<-rbind(df2,data.frame(varname=values[i],value=value,text=text,stringsAsFactors = F))
    })

    df<<-rbind(df,df2)
  })

  df
}

get.sas.labels<-function(df_xpt) {
  as.character(sapply(1:ncol(df_xpt),function(x) {
    attr(df_xpt[[x]],"label")
  }
  ))
}


#' Read BRFSS SASOUT file produced by CDC
#'
#' @param year integer: year of interest
#' @param folder character: folder location
#' @param file  character: file name
#' @param version integer: version number (defaults = 0)
#'
#' @return data frame
#' @export
#'
#'
read_sasout<-function() {

  params <- my.brfss.patterns()

  path<-apply.pattern("sas_sasout_path",params)

  if(file.exists(path)){
    lines<-readLines(path)
    lines<-gsub("\f","",lines)

    lbl_line<-grep("^Label$" ,lines)
    end_line <- max(grep("^;$" ,lines))

    if (end_line<lbl_line) end_line <- grep("^RUN;$" ,lines)
    lbl_lines<-(lbl_line+1):(end_line-1)
    label_lines<-lines[lbl_lines]

    df_lbl<-data.frame(varname=gsub("(.*) {,}=(.*)","\\1",label_lines),
                       label=gsub("(.*) {,}= {,}'(.*)'","\\2",label_lines),
                       stringsAsFactors = F)


    lines<-lines[-lbl_lines]
    inp_line<-grep("^INPUT$" ,lines)

    inp_lines<-(inp_line+1):(min(grep("^;$" ,lines))-1)
    input_lines<-lines[inp_lines]

    sect_lines<-grep("/*" ,input_lines,fixed = T)
    sects<-stringr::str_trim(gsub(".*/[*](.*)","\\1",input_lines[sect_lines]))
    sects<-stringr::str_trim(gsub("[*]/","",sects))

    sect_start<-sect_lines
    sect_end<-sect_lines[2:length(sect_lines)]-1
    sect_end[length(sect_lines)]<-length(input_lines)
    sect_length<-sect_end-sect_start+1

    sections<-character(0)
    indices<-integer(0)
    invisible(
      mapply(function(sect,len) {
        sections<<-c(sections,rep(sect,len))
        indices<<-c(indices,seq(1,len))
      },sects,sect_length )
    )
    varnames<-gsub("^([^ ]*) *.*","\\1",input_lines)

    ranges<-stringr::str_trim(gsub("^([^ ]*) *(.*?)/.*","\\2",input_lines))

    df_sections<-data.frame(section=sections,index=indices,varname=varnames,stringsAsFactors = F) #,range=ranges,stringsAsFactors = F)

    df_data<-dplyr::left_join(df_lbl,df_sections,by="varname")
    df_data$is_calculated<-grepl("Calculated Var",df_data$section)
    df_data$section_type<-""
    df_data$section_number<-""

    df_data$section_type[grep( "Section" ,df_data$section)]<- "Core"
    df_data$section_type[grep( "Module" ,df_data$section)]<- "Module"
    df_data$section_type[!grepl("[0-9]",df_data$section)]<-df_data$section[!grepl("[0-9]",df_data$section)]

    df_data$section_number<-as.integer(gsub("[^0-9]","",df_data$section))
    df_data$section_number[is.na(df_data$section_number)]<-""
    df_data$section_name<-gsub(".*:(.*)","\\1",df_data$section)

    df_data<-df_data[order(df_data$section_type,df_data$section_number,df_data$index),]
  } else {
    df_data<-data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = F)
    colnames(df_data)<-c("varname","label","section","index",             #"range",
                         "is_calculated","section_type","section_number","section_name"  )
  }

  df_data<-override(df_data)
  df_data[,c("varname","label","section_type","section_number","section_name","index","is_calculated")]
}

override<-function(df){
  df_overrides<-sasout.overrides()
  if(nrow(df_overrides)==0) return(df)

  ynfix<-which(df$varname%in%df_overrides$column)

  if(length(ynfix)==0) return(df)
  #browser()
  df0<-df[ynfix,]
  df0<- dplyr::left_join(df0,df_overrides,by=c("varname"="column"))
  df[ynfix,"label"]<-df0$label.y

  df
}

sasout.override.file<-function() {
  paste0(orrr::dir.project("data"),"sasout_overrides.RData")
}

sasout.overrides<-function() {
  file<-sasout.override.file()
  if(file.exists(file)) {
    df_override<- readRDS(file)
  } else {
    df_override<- data.frame()
  }
  df_override
}

sasout.add.override<-function(year,column,label) {
  # browser()

  df_override<- sasout.overrides()
  df_override<-df_override[df_override$year!=year & df_override$column!=column,]

  df_override<-rbind(df_override,data.frame(year,column,label))
  saveRDS(df_override,file = file)
}

sas.build.geogs <- function() {
  require(dplyr)

  df <- data.frame(Geog = state.name, Abbrev = state.abb) %>%
    mutate(name = rownames(.))



  geogs <- read.sas.format() %>%
    filter(varname == "_STATE") %>%
    rename(Geog = text, Id = value ) %>%
    select(Geog,Id) %>%
    mutate(Geog = stringr::str_trim(Geog)) %>%
    left_join(df %>% select(State, Abbrev), by = "Geog") %>%
    mutate(Abbrev = ifelse(Id == 11, "DC",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 66, "GU",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 78, "VI",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 72, "PR",Abbrev))


  usethis::use_data(geogs, overwrite = TRUE)

  geogs
}

##
##    get the sas field ranges for fwf


#' Read Data Element Ranges from SASOUT file
#'
#' These ranges can be used to read the ascii fixed width file fprovided by CDC
#'
#' @param year integer: year of interest
#'
#' @return data.frame: the ranges
#' @export
#'
#' @examples
#' \dontrun{
#' read_sas_field_ranges(2021)
#'
#' }
read_sas_field_ranges<-function() {
  require(stringr)

  params <- my.brfss.patterns()

  # get the filename for the data
  #  name format based on year
  year <- brfss.param(year)

  if(year>2010) {
    file<-apply.pattern("sas_sasout_path",params)

  } else {
    return(NULL)
    #file<-paste("sasout",sprintf("%02d",year%%100),".sas",sep="")
  }

  lines<-readLines(file, warn=F)

  lines <- gsub("—", "-",lines)

  # get the start point of interest and remove everything before


  start<-grep("^(INPUT[[:space:]]|INPUT$)",lines)

  if(nchar(lines[start]) > 10) {
    lines[start]<-gsub("^INPUT[[:space:]]","",lines[start])
  } else {
    start<-start+1
  }
  lines<-lines[start:length(lines)]



  # get the end point of interest (; by itself on a line, the first one) and remove everything after
  #   including that end line

  end<- min(grep("^;$",lines))-1
  lines<-lines[1:end ]


  ## remove known unwanted line, if they exist

  lines<-lines[grep("STATE ADDED ",lines,invert=T)]
  lines<-lines[grep("^\\*",lines,invert=T)]



  ##
  ##  get the sections/modules
  ##

  # annotated lines
  lines_secmod<- grep("[/][*]",lines)

  lines_save<-lines

  # blank unannotated lines
  lines_save[-lines_secmod]<-""

  # blank unannotated lines
  lines_save<-str_trim(gsub(".*[/][*] (.*)","\\1",lines_save))
  lines_save<-str_trim(gsub("[*][/]","",lines_save))

  #####################################################################
  ##
  ##    get the section
  type <- ""
  section <- sapply(lines_save, function(typ) {
    if(typ!="") type<<-typ
    return(type)

  })

  section <- stringr::str_trim(unname(gsub(".*:","",section)))

  # lines of interest ... beginning of Section or Module
  lines_secmod2<- grep("^(Sec|Mod).*:",lines_save)

  # put random word ('STOP') at postion where not a question
  lines_save[lines_secmod[!lines_secmod%in%lines_secmod2]]<-"STOP"
  lines_save[grep("[Vv]ariable",lines_save)]<-"STOP"
  lines_save[grep("Questionnaire",lines_save)]<-"STOP"

  lines_stop<-grep("^STOP$",lines_save)

  #beginning (1st question) of Section
  sect_start<-grep("^Section ",lines_save)
  sect_end<-sapply(sect_start,function(x) min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1)

  #beginning (1st question) of Module
  mod_start<-grep("^Module ",lines_save)
  mod_end<-sapply(mod_start,function(x) min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1)

  section_text<-character(length(lines_save))
  section_index<-integer(length(lines_save))

  mapply(function(s0,s1) {
    section_text[s0:s1]<<-str_trim(gsub("Section (.*):(.*)[*][/]","\\2",lines_save[s0]))
    section_index[s0:s1]<<-(s0:s1)-s0+1

  },sect_start,sect_end)

  module_text<-character(length(lines_save))
  module_index<-integer(length(lines_save))

  mapply(function(m0,m1) {
    module_text[m0:m1]<<-str_trim(gsub("Module (.*):(.*)[*][/]","\\2",lines_save[m0]))
    module_index[m0:m1]<<-(m0:m1)-m0+1

  },mod_start,mod_end)

  df_sectmod<-data.frame(section_text,section_index,module_text,module_index)

  #########

  lines<-str_trim(gsub("[/][*].*","",lines))

  keep<-!grepl("[*][/]",lines)
  lines<-lines[keep]
  df_sectmod<-df_sectmod[keep,]

  vars<-gsub("(.*)[[:space:]](.*)","\\1",lines)
  #vars<-str_trim(gsub("^_","X_",vars))
  vars<-str_trim(vars)

  range<-gsub("(.*)[[:space:]](.*)","\\2",lines)
  range<-gsub("[$]","",range)
  start<-as.integer(gsub("(.*)-.*","\\1",range))
  end<-as.integer(gsub("(.*)-(.*)","\\2",range))

  df<-data.frame(var=vars,start=start,end=end,stringsAsFactors = F)

  df<-cbind(df,df_sectmod) %>%
    mutate(sect_type = ifelse(section_index >0,"Core",""))  %>%
    mutate(sect_type = ifelse(module_index >0,"Module",sect_type)) %>%
    mutate(sect_type = ifelse(sect_type == "" , section, sect_type)) %>%
    mutate(sect_num = ifelse(section_index>module_index,section_index,module_index)) %>%
    mutate(section = section)

  df<-df[!is.na(df$start),]


  df
}

fill_dummies <- function(df) {
  require(dplyr)

  start <- as.integer(df%>% slice(2:nrow(.)) %>% pull(start))
  last <-  as.integer(df%>% slice(1:(nrow(.)-1)) %>% pull(end))
  fillem <- ((start-last)>1)
  df_fill <- data.frame(start = last+1, end = start -1 ) %>%
    mutate(field_size = end - start + 1) %>%
    filter(fillem) %>%
    relocate(field_size , 1) %>%
    # mutate(start = as.character(start), end = as.character(end)) %>%
    mutate(col_name = paste0("DUMMY_",start)) %>%
    mutate(sect_type = "DUMMY") %>%
    mutate(section = "DUMMY") %>%
    bind_rows(df) %>%
    arrange(as.integer(start))

  df_fill
}
