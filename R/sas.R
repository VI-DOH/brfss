

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

unzip.all<-function(year,rmzip=TRUE) {
  folder<-apply.pattern("sas_raw_folder",YEAR =year)
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
  save(list=ls(e),file =fname )

}

#' Process BRFSS Annual Data Files
#' Use this function to process a single year of BRFSS data. This function sill download, unzip,
#' and create a data frame with all data from the XPT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param download - logical - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param xpt - logical - read/parse the xpt file? Useful (set = FALSE) if you already have the xpt files processed
#' @param codebook - logical - download the annual codebook
#' @param split - logical - split the xpt file by state/geography
#' @param ... other params
#'
#' @return invisible()
#' @export
#'
#' @examples
sas.process.year<-function(year,download=TRUE,xpt=TRUE, codebook = TRUE, split = TRUE, verbose=FALSE, ...) {

  if(missing(year)) year <- my.year()

  if(download) {
    if(verbose) cat(" ... downloading ... main file ... ")
    sas.download.data(year=year)
    if(verbose) cat(" versions ")
    sas.download.data.versions(year=year)
    if(verbose) cat(" \n ... unzipping files\n ")
    unzip.all(year=year)
  }


  if(xpt) {
    if(verbose) cat(" ... reading main xpt file\n ")
    read.xpt(year=year)

    ivers<-1
    if(verbose) cat(" ... trying versions\n ")
    while (read.xpt(year=year,version=ivers,verbose=TRUE)) {
      #browser()
      if(verbose) cat("Read version=",ivers,"\n")
      ivers<-ivers + 1
    }
  }
  browser()
  save.sas.layout(year = year)
  #sas.save.sasout(year = year)

  if(codebook) {
    download.codebook(year = year)
    save_codebook_layout(year = year)
  }

  if(split) cleave.geogs(year=year,...)

  save_response_stats(year = year)
  save_module_stats(year = year)

  invisible()
}

sas.save.sasout<-function(year) {

  fname<-paste0("df_columns_",year)
  assign(fname,read.sasout(year))
  file <- apply.pattern("brfss_columns_path", YEAR=year)

  if(!dir.exists(dirname(file))) dir.create(dirname(file))

  save(list=fname,file = file)
}


sas.download.data<-function(year) {

  #  files<-sas.url.pattern.downloads.data()
  files<-get.pattern.group("sas_downloads")
  urlfiles<- apply.pattern("brfss_url_files",YEAR=year)
  folderout<-apply.pattern("sas_raw_folder",YEAR=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  sapply(files,function(file) {
    file<-patternize(strIn = file,YEAR = year)
    url<-paste0(urlfiles,file)
    fileout<-paste0(folderout,file)

    #
    # download will fail on lager files if time to download is > 60 secs
    #   set time to 3 minutes and then restore when done

    to <- getOption("timeout")
    options(timeout = 180)

    download.file(url = url,destfile = fileout,
                  method = "libcurl")

    options(timeout = to)

    if(grepl("[.]zip$",fileout)) {
      unzip(fileout,exdir = normalizePath(folderout))
      file.remove(fileout)
    }
  })

}


sas.download.data.versions<-function(year) {

  #files<-sas.url.pattern.downloads.versions()
  files<-get.pattern.group("sas_version_downloads")

  folderout<-apply.pattern("sas_raw_folder",YEAR=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles<-apply.pattern("brfss_url_files",YEAR=year)

  sapply(files,function(file) {
    done <- FALSE
    sapply(1:6,function(version) {

      file<-patternize(strIn = file,YEAR = year,VERS = version )
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
#' This function can get the default file names folder location from the file_patterns data.
#'
#' @param year integer: year of interest
#' @param version interger: version of interest (default is 0 ... main survey)
#' @param xpt_folder character: location (folder) of xpt file
#' @param xpt_file character:  name of xpt file
#' @param save_folder character:location (folder) to save .rda file to
#' @param save_file character: name of .rda file
#' @param sasout_folder character: location (folder) of sasout file
#' @param sasout_file character: filename of sasout file
#' @param verbose logical: provide extra information during processing
#'
#' @return
#' @export
#'

#' @examples
#' \dontrun{
#' df_2021 <- read.xpt(2021)
#'
#' df_data <- read.xpt(2020, version= 1,
#' xpt_folder = "./data_raw/2020/sas/", xpt_file = "LLCP20V1.XPT",
#' save_folder = "./data/2020/", save_file = "brfss2020.rda",
#' sasout_folder = "./data_raw/2020/sas/", sasout_file = "SASOUT20_LLCP.SAS")
#' }

read.xpt<-function(year,version = 0,
                   xpt_folder = NULL,
                   xpt_file = NULL,
                   save_folder = NULL,
                   save_file = NULL,
                   sasout_folder = NULL,
                   sasout_file = NULL,
                   verbose = F) {

  ########################################################################%%%%%%%%%
  ##
  ##    If file and folder names not supplied, create them from the file patterns

  ##
  ##    get sasout location
  ##


  if(is.null(sasout_file)) {
    if(version>0) {

      sasout_file<-apply.pattern("sas_sasout_version",YEAR = year, VERS = version)
    } else {
      sasout_file<-apply.pattern("sas_sasout",YEAR = year)
    }
  }

  if(is.null(sasout_folder)) {
    sasout_folder <- apply.pattern("sas_raw_folder",YEAR = year)
  }

  ##
  ##    get xpt raw data location
  ##

  if(is.null(xpt_folder)) {
    xpt_folder <- apply.pattern("sas_raw_folder", YEAR = year)
  }

  if(is.null(xpt_file)) {
    if(version>0) {
      xpt_file <- apply.pattern("xpt_file_version",YEAR = year, VERS = version)
    } else {
      xpt_file <- apply.pattern("xpt_file",YEAR = year)
    }
  }

  ##
  ##  get save file (.rdata) location
  ##

  if(is.null(save_file)) {
    if(version == 0) {
      save_file <- apply.pattern("sas_rdata",YEAR = year)
    } else {
      save_file<- apply.pattern("sas_rdata_version",YEAR = year, VERS = version)
    }
  }

  if(is.null(save_folder)) {
    save_folder <- apply.pattern("sas_data_folder",YEAR = year)
  }

  ##
  ##    read the xpt files
  ##


  if(version>0) {
    if(verbose) cat("Trying version ",version,"\n")

  } else {
    if(verbose) cat("Trying main file \n")
  }

  xptname<-paste0(xpt_folder,xpt_file)

  if(file.exists(xptname)) {
    if(verbose) cat("Reading ",xptname,"\n")
    df_xpt<- haven::read_xpt(xptname)

    cat("Getting sasout\n")

    sasout_path <- paste0(sasout_folder,sasout_file)
    if(verbose) cat("Trying sasout file: ",sasout_path,"\n")
    if(file.exists(sasout_path)) {
      if(verbose) cat("Sasout file [",sasout_path,"] exists ... reading\n")
      df_sasout<-read.sasout(year,sasout_folder,sasout_file)
      if(verbose) cat("Sasout file returned [",nrow(df_sasout),"] records \n")
      #sink(file = "./errtest.txt")
      mapply(function(lbl,v,typ,n,i,nm) {
        #cat(v,"|",typ,"|",n,"|",i,"|",nm)
        if(!is.null(df_xpt[[v]])) {
          if(is.na(typ) || is.null(typ)) typ<=""
          if(is.na(n) || is.null(n)) n<=""
          if(is.na(i) || is.null(i)) i<=""
          if(is.na(nm) || is.null(nm)) nm<=""
          attr(df_xpt[[v]],"section_type")<<-typ
          attr(df_xpt[[v]],"section_num")<<-n
          attr(df_xpt[[v]],"section_index")<<-i
          attr(df_xpt[[v]],"section_name")<<-stringr::str_trim(nm)
          attr(df_xpt[[v]],"label")<<-lbl
        } else {
          #cat(" ... NULL!")
        }
        #cat("\n")
      }, df_sasout$label, df_sasout$varname, df_sasout$section_type, df_sasout$section_number,
      df_sasout$index,df_sasout$section_name)
    }
    #cat("Saving\n")
    #sink(file = NULL)

    if(!dir.exists(save_folder)) dir.create(save_folder,recursive = T)

    fname<-paste0("df_xpt_",year,ifelse(version>0,paste0("_V",version),""))
    assign(fname,df_xpt)
    save(list=c(fname),file = paste0(save_folder,save_file))
    return(TRUE)
  } else {
    if(verbose) cat(xptname," doesn't exist\n")
    return(FALSE)
  }
}


#' Split BRFSS XPT by Geography
#'
#' The main BRFSS XPT data file created when the XPT file is downloaded and read has data
#' for all geographies.This function splits out the geographies of interest.
#'
#' @param year integer - year of interest
#' @param rdata_folder character - location of .RData file with full BRFSS XPT data
#' @param rdata_file character - filename of .RData file with full BRFSS XPT data
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
#' cleave.geogs(year = 2020,rdata_folder="./data/2020/", rdata_file="xpt_2020.RData",
#'    main=TRUE,versions=TRUE, my_geog="MT", other_geogs=NULL,verbose=TRUE)
#'}
#'
cleave.geogs<-function(year = NULL, rdata_folder=NULL, rdata_file=NULL,
                      main=TRUE,versions=TRUE, my_geog=NULL, other_geogs=NULL,verbose=TRUE) {

  if(!(main || versions)) return(NULL)

  year <- get.year(year)

  if(is.null(my_geog)) my_geog <- my.geog()
  if(is.null(other_geogs)) other_geogs <- my.other.geogs()

  if(my_geog=="") my_geog <- character(0)

  geogs <- get.geogs()

  ver<-integer(0)
  if(main) ver<-0

  vermax <- highest_version(year)

  if(versions) ver<-c(ver,1:vermax)

  df_geogs <- get.geogs.all()

  sapply(ver,function(version) {

    df_xpt<-load.sas(year,rdata_folder = rdata_folder,rdata_file = rdata_file, version)

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

    # fldr_geog <- normalizePath(apply.pattern("brfss_geog_folder", YEAR = year, GEOG= geog),
    #                            winslash = "/", mustWork = FALSE)


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

          if(str_something(my_geog) &&  (nm == my_geog)) {
            fldr <- apply.pattern("brfss_annual_data_folder",  YEAR = year)
          } else {
            fldr <- apply.pattern("brfss_geog_folder",  YEAR = year, GEOG = nm)
          }

          if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)

          if(version == 0) {

            file <- apply.pattern("brfss_geog_file",  YEAR = year, GEOG = nm)

          } else {

            file <- apply.pattern("brfss_geog_file_version",  YEAR = year, GEOG = nm, VERS = version)

          }

          fname <- paste0(fldr,file)
          if(verbose) cat("Going to save :", fname, "\n")

          save(list = c(dfname),file = fname)

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

    save(list=ls(e),file =fname )
  }
}

load.sas<-function(year, rdata_folder=NULL,
                   rdata_file=NULL, version=0) {

  ##
  ##  get save file (.rdata) location
  ##

  if(is.null(rdata_file)) {
    if(version == 0) {
      rdata_file <- apply.pattern("sas_rdata",YEAR = year)
    } else {
      rdata_file<- apply.pattern("sas_rdata_version",YEAR = year, VERS = version)
    }
  }

  if(is.null(rdata_folder)) {
    rdata_folder <- apply.pattern("sas_data_folder",YEAR = year)
  }

  e<-new.env()

  load(file = paste0(rdata_folder,rdata_file),envir = e)
  get(ls(e),envir = e)
}


read.sas.format<-function(year,folder_pat= NULL, file_pat = NULL) {
  require(dplyr)

  if(is.null(folder_pat)) folder_pat <- get.pattern("sas_raw_folder") %>% expand.pattern()
  if(is.null(file_pat)) file_pat <- expand.pattern(get.pattern("sas_file_format"))

  folder<-patternize(folder_pat, YEAR = year)
  file<-patternize(file_pat, YEAR = year)
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
read.sasout<-function(year,folder=NULL,file=NULL,version=0) {

  if(is.null(file)) {
    if(version>0) {

      file<-apply.pattern("sas_sasout_version",YEAR = year, VERS = version)
    } else {
      file<-apply.pattern("sas_sasout",YEAR = year)
    }
  }

  if(is.null(folder)) {
    folder <- apply.pattern("sas_raw_folder",YEAR = year)
  }


  path<-paste0(folder,file)
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
    df_override<- orrr::get.rdata(file)
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
  save(df_override,file = file)
}

sas.build.geogs <- function(year) {
  require(dplyr)

  df <- data.frame(Geog = state.name, Abbrev = state.abb) %>%
    mutate(name = rownames(.))



  geogs <- read.sas.format(year) %>%
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


#' Get BRFSS layout from SASout
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
save.sas.layout<-function(year = NULL) {
  require(stringr)

  year <- get.year(year)

  # get the filename for the data
  #  name format based on year
  if(year>2010) {
    file<-apply.pattern("sas_sasout_path",YEAR=year)

  } else {
    return(NULL)
    #file<-paste("sasout",sprintf("%02d",year%%100),".sas",sep="")
  }


  #  file <- paste0(orrr::dir.project(),"/data/layout/sas/",file)
  lines<-readLines(file, warn = F, encoding = "latin1")

  lines <- gsub("—", "-",lines)

  # get the start point of interest and remove everything before

  start<-grep("^Label$",lines) + 1
  if (length(start)==0) start<-grep("\\* ASSIGN ",lines)
  lines<-lines[start:length(lines)]

  # get the end point of interest (; by itself on a line) and remove everything after
  #   including that end line

  lines<-lines[1:(grep("^;$",lines)-1) ]


  #####################################
  ##
  ##  put together broken lines

  varlines<-grep(" = '", lines)
  vline2<-c(varlines[2:length(varlines)]-1,length(lines))

  ulines<-as.character(mapply(function(l0,l1) {
    paste(lines[l0:l1],collapse="")

  },varlines,vline2))

  vars<-gsub("(.*) =.*","\\1",ulines)
  vars<-str_trim(vars)
  vars<-gsub("^_","X_",vars)


  question<-gsub("(.*) = '(.*)'$","\\2",ulines)

  df_ranges<-read.sas.field.ranges(year)

  df<- dplyr::left_join(df_ranges,data.frame(var=vars,description=question,stringsAsFactors = F),by="var")

  colnames(df)[colnames(df)=="var"]<-"col_name"
  df$field_size<-as.integer(df$end)-as.integer(df$start)+1

  df_layout <- df %>% select(field_size,start,end,col_name,  group_type, group_number,
                             group_text, description) %>%
    group_by(group_text) %>%
    mutate(group_index = row_number()) %>%
    relocate(group_index,.before = description) %>%
    as.data.frame() %>%
    fill_dummies()

  fldr <- apply.pattern("brfss_layout_folder",YEAR = year)
  if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)

  save(df_layout, file = apply.pattern("brfss_layout_path",YEAR = year))
  invisible()
}
#####################################################################################################
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
#' read.sas.field.ranges(2021)
#'
#' }
read.sas.field.ranges<-function(year) {
  require(stringr)

  # get the filename for the data
  #  name format based on year

  if(year>2010) {
    file<-apply.pattern("sas_sasout_path",YEAR=year)

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
  ##    get the group_text
  type <- ""
  group_text <- sapply(lines_save, function(typ) {
    if(typ!="") type<<-typ
    return(type)

  })

  group_text <- unname(gsub(".*:","",group_text))

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
  vars<-str_trim(gsub("^_","X_",vars))

  range<-gsub("(.*)[[:space:]](.*)","\\2",lines)
  range<-gsub("[$]","",range)
  start<-gsub("(.*)-.*","\\1",range)
  end<-gsub("(.*)-(.*)","\\2",range)

  df<-data.frame(var=vars,start=start,end=end,stringsAsFactors = F)

  df<-cbind(df,df_sectmod) %>%
    mutate(group_type = ifelse(section_index >0,"Core",""))  %>%
    mutate(group_type = ifelse(module_index >0,"Module",group_type)) %>%
    mutate(group_type = ifelse(group_type == "" , group_text, group_type)) %>%
    mutate(group_number = ifelse(section_index>module_index,section_index,module_index)) %>%
    mutate(group_text = group_text)

  df<-df[!is.na(as.integer(df$start)),]


  df
}

fill_dummies <- function(df) {
  require(dplyr)

  start <- as.integer(df%>% slice(2:nrow(.)) %>% pull(start))
  last <-  as.integer(df%>% slice(1:(nrow(.)-1)) %>% pull(end))
  fillem <- ((start-last)>1)
  df_fill <- data.frame(start = last+1, end = start -1 ) %>%
    mutate(field_size = end - start) %>%
    filter(fillem) %>% relocate(field_size , 1) %>%
    mutate(start = as.character(start), end = as.character(end)) %>%
    mutate(col_name = paste0("DUMMY",start)) %>%
    mutate(group_type = "DUMMY") %>%
    mutate(group_text = "DUMMY") %>%
    bind_rows(df) %>%
    arrange(as.integer(start))

  df_fill
}
