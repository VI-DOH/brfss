

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


# sas.url.pattern.downloads.data<-function() {
#   c(
#     "LLCP[YEAR]XPT.zip",
#     "SASOUT[YR]_LLCP.SAS",
#     "Format[YR].sas",
#     "Formas[YR].sas"
#   )
# }

# sas.url.pattern.downloads.versions<-function() {
#
#   c(
#     "LLCP[YR]V[VERS]_XPT.zip",
#     "SASOUT[YR]_LLCP_V[VERS].SAS"
#   )
# }
#
# sas.folder.pattern.raw_data<-function(){
#   "./data_raw/[YEAR]/sas/"
# }
#
# sas.folder.pattern.data<-function(){
#   "./data/[YEAR]/"
# }
#
# sas.file.pattern.rdata<-function(){
#   "xpt_[YEAR].RData"
# }
#
# sas.file.pattern.xpt<-function(){
#   "LLCP[YEAR].XPT"
# }
#
# sas.file.pattern.format<-function(){
#   "FORMAT[YR].sas"
# }
# #
#  sas.file.pattern.sasout<-function(version=0){
#   #  https://www.cdc.gov/brfss/annual_data/2017/files/SASOUT17_LLCP_V3.SAS
#   ptrn<-"SASOUT[YR]_LLCP.SAS"
#   if(version>0) ptrn<-gsub("P.SAS",paste0("P_V",version,".SAS"),ptrn,fixed = T)
#   ptrn
# }

unzip.all<-function(year,rmzip=TRUE) {
  folder<-apply.pattern("sas_folder_raw",year)
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
#' Use this function to process a single year of BRFSS data. This function sill download, unzip, and create a data frame with all data from the XPT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param download - boolean - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param xpt - boolean - read/parse teh xpt file? Useful (set = FALSE) if you already have the xpt files processed
#' @param verbose - output some information during processing
#' @param ... other params
#'
#' @return invisible()
#' @export
#'
#' @examples
sas.process.year<-function(year,download=TRUE,xpt=TRUE, verbose=FALSE, ...) {

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
  sas.save.sasout(year = year)
  split.states(year=year,...)

  save_response_stats(year = year)
  save_module_stats(year=year)

  invisible()
}

sas.save.sasout<-function(year) {

  fname<-paste0("df_columns_",year)
  assign(fname,read.sasout(year))

  save(list=fname,file = paste0("./data/",year,"/columns_",year,".RData"))
}


sas.download.data<-function(year) {

  #  files<-sas.url.pattern.downloads.data()
  files<-get.pattern("sas_downloads")
  urlfiles<- apply.pattern("brfss_url_files",year=year)
  folderout<-apply.pattern("sas_folder_raw",year=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  sapply(files,function(file) {
    file<-patternize(strIn = file,year = year)
    url<-paste0(urlfiles,file)
    fileout<-paste0(folderout,file)
    download.file(url = url,destfile = fileout)
    if(grepl("[.]zip$",fileout)) {
      unzip(fileout,exdir = normalizePath(folderout))
      file.remove(fileout)
    }
  })

}


sas.download.data.versions<-function(year) {

  #files<-sas.url.pattern.downloads.versions()
  files<-get.pattern("sas_version_downloads")

  folderout<-apply.pattern("sas_folder_raw",year=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles<-apply.pattern("brfss_url_files",year=year)

  sapply(files,function(file) {
    done <- FALSE
    sapply(1:6,function(version) {

      file<-patternize(strIn = file,year = year,version = version )
      url<-paste0(urlfiles,file)
      if(!done) { #RCurl::url.exists(url)){
        fileout<-paste0(folderout,file)

        oldw <- getOption("warn")
        options(warn = -1)
        #      for (file in files) {

        tryCatch(expr = {
          download.file(url = url,destfile = fileout)
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

read.xpt<-function(year,
                   readfolder_pat=get.pattern("sas_folder_raw"),
                   file_pat=get.pattern("xpt_file"),
                   savefolder_pat=get.pattern("sas_folder_data"),
                   save_pat=get.pattern("sas_rdata"),
                   sasout_folder_pat=get.pattern("sas_folder_raw"),
                   sasout_file_pat=NULL,
                   version=0,verbose=F) {

  if(is.null(sasout_file_pat)) {
    if(version>0) sasout_file_pat<-apply.pattern("sas_sasout_version",year = year, version = version) else
      sasout_file_pat<-apply.pattern("sas_sasout",year = year)
  }

  folder<-patternize(readfolder_pat,year)
  if(version>0) {
    if(verbose) cat("Trying version ",version,"\n")
    file_pat<-"LLCP[YR]V[VERS].XPT"
    save_pat<-"xpt_[YEAR]_V[VERS].RData"

  } else {
    if(verbose) cat("Trying main file \n")
  }

  file<-patternize(file_pat,year = year,version = version)
  xptname<-paste0(folder,file)

  if(file.exists(xptname)) {
    if(verbose) cat("Reading ",xptname,"\n")
    df_xpt<- haven::read_xpt(xptname)

    cat("Getting sasout\n")

    df_sasout<-read.sasout(year,sasout_folder_pat,sasout_file_pat)
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
    }, df_sasout$label,df_sasout$varname,df_sasout$section_type,df_sasout$section_number,df_sasout$index,df_sasout$section_name)

    #cat("Saving\n")
    #sink(file = NULL)

    savefolder<-patternize(savefolder_pat,year)
    if(!dir.exists(savefolder)) dir.create(savefolder,recursive = T)
    savename<- patternize(save_pat,year,version=version)
    fname<-paste0("df_xpt_",year,ifelse(version>0,paste0("_V",version),""))
    assign(fname,df_xpt)
    save(list=c(fname),file = paste0(savefolder,savename))
    return(TRUE)
  } else {
    if(verbose) cat(xptname," doesn't exist\n")
    return(FALSE)
  }
}


split.states<-function(year, loadfolder_pat=get.pattern("sas_folder_data"),
                       load_pat=get.pattern("sas_rdata"),main=TRUE,versions=TRUE,states=NULL,verbose=TRUE) {

  ver<-integer(0)
  if(main) ver<-0
  if(versions) ver<-c(ver,1:3)

  df_states <- orrr::get.rdata("./data/states.RData")

  sapply(ver,function(version) {

    fname<-paste0("df_xpt_",year)

    if (version>0) {
      fname<-paste0(fname,"_",version)
      load_pat<-"xpt_[YEAR].RData"
    }

    #if(!exists(fname))
    df_xpt<-load.sas(year,loadfolder_pat,load_pat,version)

    if(is.null(states)) {
      states<-unique(df_xpt$`_STATE`)

    } else {
      if(is.character(states)) {
        states<-sapply(states,function(state) {
          df_states[df_states$Abbrev==state,"Id"]
        })
      }
    }

    add_cols<-character(0)

    fldr_geog <- paste0("./data/",year,"/geog/")

    if(!dir.exists(fldr_geog)) dir.create(fldr_geog)

    mapply(function(id,nm) {
      cat("X\n")


      if(id%in%states) {

        if(verbose) cat("Saving ",nm,"V",version,"\n")
        df_state<-df_xpt[df_xpt$`_STATE`==id,]
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

        fname<-paste0("./data/",year,"/geog/",nm,"_",year,".RData")
        if(version>0) fname<-gsub("[.]RData",paste0("_V",version,".RData"),fname)

        save(list = c(dfname),file = fname)
        #browser()
        columns.add(year,add_cols)
      }
    },df_states$Id,df_states$Abbrev)
  })

}

columns.add<-function(year,cols2add){
  e<-new.env()
  fname<-apply.pattern("brfss_columns_file",year)
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

load.sas<-function(year, loadfolder_pat=sas.folder.pattern.data(),
                   load_pat=sas.file.pattern.rdata(),version=0) {

  e<-new.env()
  loadfolder<-patternize(loadfolder_pat,year)
  loadname<- patternize(load_pat,year)
  if(version>0) loadname<-gsub("[.]RData",paste0("_V",version,".RData"),loadname)
  #browser()
  load(file = paste0(loadfolder,loadname),envir = e)
  get(ls(e),envir = e)
}


read.sas.format<-function(year,folder_pat= NULL, file_pat = NULL) {

  if(is.null(folder_pat)) folder_pat <- get.pattern("sas_folder_raw")
  if(is.null(file_pat)) file_pat <- get.pattern("sas_file_format")

  folder<-patternize(folder_pat,year)
  file<-patternize(file_pat,year)
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


read.sasout<-function(year,folder_pat=NULL,file_pat=NULL,version=0) {
  if(is.null(folder_pat)) folder_pat <- get.pattern("sas_folder_raw")
  folder<-patternize(folder_pat,year)

  if(is.null(file_pat)) {
    if(version>0) file_pat<-get.pattern("sas_sasout_version") else
      file_pat<-get.pattern("sas_sasout")
  }

  file<-patternize(file_pat,year,version = version)
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

sas.build.states <- function(year) {
  require(dplyr)

  df <- data.frame(State = state.name, Abbrev = state.abb) %>%
    mutate(name = rownames(.))



  df_states <- read.sas.format(year) %>%
    filter(varname == "_STATE") %>%
    rename(State = text, Id = value ) %>%
    select(State,Id) %>%
    mutate(State = stringr::str_trim(State)) %>%
    left_join(df %>% select(State, Abbrev), by = "State") %>%
    mutate(Abbrev = ifelse(Id == 11, "DC",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 66, "GU",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 78, "VI",Abbrev))%>%
    mutate(Abbrev = ifelse(Id == 72, "PR",Abbrev))


  save(df_states, file = "./data/states.Rdata")
  df_states
}


read.sas.layout<-function(year, save = TRUE) {
  require(stringr)

  # get the filename for the data
  #  name format based on year
  if(year>2010) {
    file<-apply.pattern("sas_sasout_path",year=year)

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

  if(save) save(df_layout, file = apply.pattern("brfss_layout_path",year = year))
  df_layout
}
#####################################################################################################
##
##    get the sas field ranges for fwf


read.sas.field.ranges<-function(year) {
  require(stringr)

  # get the filename for the data
  #  name format based on year

  if(year>2010) {
    file<-apply.pattern("sas_sasout_path",year=year)

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
