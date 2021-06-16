################################
sub_pattern<-function(strIn, year, version="") {
  ret<-gsub("[YEAR]",year,strIn,fixed = T)
  ret<-gsub("[YR]",year%%100,ret,fixed = T)
  gsub("\\[VERS\\]",version,ret)
}

brfss.url.pattern.files<-function() {
  "https://www.cdc.gov/brfss/annual_data/[YEAR]/files/"
}

brfss.url.pattern.documentation<-function() {
  "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/"
}

#https://www.cdc.gov/brfss/annual_data/2016/pdf/2016_calculated_variables_version4.pdf
sas.url.pattern.download.doc<-function() {
  c(
    "codebook[YR]_llcp.pdf"
  )
}

sas.url.pattern.downloads.data<-function() {
  c(
    "LLCP[YEAR]XPT.zip",
    "SASOUT[YR]_LLCP.SAS",
    "Format[YR].sas"
  )
}

sas.url.pattern.downloads.versions<-function() {

  c(
    "LLCP[YR]V[VERS]_XPT.zip",
    "SASOUT[YR]_LLCP_V[VERS].SAS"
  )
}
sas.folder.pattern.raw_data<-function(){
  "./data_raw/[YEAR]/sas/"
}

sas.folder.pattern.data<-function(){
  "./data/[YEAR]/"
}

sas.file.pattern.rdata<-function(){
  "xpt_[YEAR].RData"
}

sas.file.pattern.xpt<-function(){
  "LLCP[YEAR].XPT"
}

sas.file.pattern.format<-function(){
  "FORMAT[YR].sas"
}

sas.file.pattern.sasout<-function(){
  "SASOUT[YR]_LLCP.sas"
}

unzip.all<-function(year) {
  folder<-sub_pattern(strIn =sas.folder.pattern.raw_data(),year)
  files<-list.files(folder,full.names = T)

  files<-files[grep("[.]zip",files)]
  folder<-normalizePath(folder,winslash = "/")
  folder<-gsub("[/]$","",folder)
  invisible(
    sapply(files, function(file) {
      #browser()
      unzip(zipfile = file, exdir = folder,overwrite = T)
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

sas.process.year<-function(year,download=TRUE) {

  if(download) {
    sas.download.data(year=year)
    sas.download.data.versions(year=year)
    unzip.all(year=year)
  }
  read.xpt(year=year)

  ivers<-1
  while (read.xpt(year=year,version=ivers,verbose=TRUE)) {
    #browser()
    cat("Read ivers=",ivers,"\n")
    ivers<-ivers+1
  }

  sas.save.sasout(year = year)
  split.states(year=year)
}

sas.save.sasout<-function(year) {

  fname<-paste0("df_columns_",year)
  assign(fname,read.sasout(year))

  save(list=fname,file = paste0("./data/",year,"/columns_",year,".RData"))
}

sas.download.data<-function(year) {

  files<-sas.url.pattern.downloads.data()
  urlfiles<-sub_pattern(strIn =brfss.url.pattern.files(),year=year)
  folderout<-sub_pattern(strIn =sas.folder.pattern.raw_data(),year=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  sapply(files,function(file) {
    file<-sub_pattern(strIn = file,year = year)
    url<-paste0(urlfiles,file)
    fileout<-paste0(folderout,file)
    download.file(url = url,destfile = fileout)
  })

}


sas.download.data.versions<-function(year) {

  files<-sas.url.pattern.downloads.versions()
  folderout<-sub_pattern(strIn =sas.folder.pattern.raw_data(),year=year)
  if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

  urlfiles<-sub_pattern(strIn =brfss.url.pattern.files(),year=year)

  sapply(files,function(file) {
    sapply(1:4,function(version) {
      file<-sub_pattern(strIn = file,year = year,version = version )
      url<-paste0(urlfiles,file)
      if( TRUE) {   #RCurl::url.exists(url)){
        fileout<-paste0(folderout,file)
        download.file(url = url,destfile = fileout)
      }
    })
  })

}

read.xpt<-function(year,readfolder_pat=sas.folder.pattern.raw_data(),
                   file_pat=sas.file.pattern.xpt(),
                   savefolder_pat=sas.folder.pattern.data(),
                   save_pat=sas.file.pattern.rdata(),
                   sasout_folder_pat=sas.folder.pattern.raw_data(),
                   sasout_file_pat=sas.file.pattern.sasout(),
                   version=0,verbose=F) {

  folder<-sub_pattern(readfolder_pat,year)
  if(version>0) {
    if(verbose) cat("Trying version ",version,"\n")
    file_pat<-"LLCP[YR]V[VERS].XPT"
    save_pat<-"xpt_[YEAR]_V[VERS].RData"

  } else {
    if(verbose) cat("Trying main file \n")
  }

  file<-sub_pattern(file_pat,year = year,version = version)
  xptname<-paste0(folder,file)

  if(file.exists(xptname)) {
    if(verbose) cat("Reading ",xptname,"\n")
    df_xpt<- haven::read_xpt(xptname)

    cat("Getting sasout\n")

    df_sasout<-read.sasout(year,sasout_folder_pat,sasout_file_pat)
    #sink(file = "./errtest.txt")
    mapply(function(v,typ,n,i,nm) {
      #cat(v,"|",typ,"|",n,"|",i,"|",nm)
      if(!is.null(df_xpt[[v]])) {
        if(is.na(typ) || is.null(typ)) typ<=""
        if(is.na(n) || is.null(n)) n<=""
        if(is.na(i) || is.null(i)) i<=""
        if(is.na(nm) || is.null(nm)) nm<=""
        attr(df_xpt[[v]],"section_type")<<-typ
        attr(df_xpt[[v]],"section_num")<<-n
        attr(df_xpt[[v]],"section_index")<<-i
        attr(df_xpt[[v]],"section_name")<<-nm
      } else {
        #cat(" ... NULL!")
      }
      #cat("\n")
    }, df_sasout$varname,df_sasout$section_type,df_sasout$section_number,df_sasout$index,df_sasout$section_name)

    #cat("Saving\n")
    sink(file = NULL)

    savefolder<-sub_pattern(savefolder_pat,year)
    if(!dir.exists(savefolder)) dir.create(savefolder,recursive = T)
    savename<- sub_pattern(save_pat,year,version=version)
    fname<-paste0("df_xpt_",year,ifelse(version>0,paste0("_V",version),""))
    assign(fname,df_xpt)
    save(list=c(fname),file = paste0(savefolder,savename))
    return(TRUE)
  } else {
    if(verbose) cat(xptname," doesn't exist\n")
    return(FALSE)
  }
}

split.states<-function(year, loadfolder_pat=sas.folder.pattern.data(),
                       load_pat=sas.file.pattern.rdata(),main=TRUE,versions=TRUE) {

  ver<-integer(0)
  if(main) ver<-0
  if(versions) ver<-c(ver,1:3)

  sapply(ver,function(version) {
    fname<-paste0("df_xpt_",year)

    if (version>0) {
      fname<-paste0(fname,"_",version)
      load_pat<-"xpt_[YEAR].RData"
    }

    #if(!exists(fname))
    df_xpt<-load.sas(year,loadfolder_pat,load_pat,version)

    states<-unique(df_xpt$`_STATE`)
    load("./data/states.RData")

    add_cols<-character(0)

    mapply(function(id,nm) {
      if(id%in%states) {
        #browser()
        cat("Saving ",nm,"V",version,"\n")
        df_state<-df_xpt[df_xpt$`_STATE`==id,]
        sapply(1:ncol(df_xpt),function(i) {
          attrs<-attributes(df_xpt[[i]])
          # cat("============================================================\n")
          # cat(colnames(df_state)[i],"\n")
          # cat(paste(names(attrs),collapse = "|"),"\n")
          # cat(paste(attrs,collapse = "|"),"\n")
          if(!is.null(attrs)){
            sapply(1:length(attrs),function(j) {
              #browser()
              attr(df_state[[i]],names(attrs[j]))<<-attr(df_xpt[[i]],names(attrs[j]))
            })
          } else {
            add_cols<<-c(add_cols,colnames(df_xpt)[i])
          }

        })
        dfname<-paste0("df_",nm)
        if(version>0) dfname<-gsub(nm,paste0(nm,"_V",version),dfname)

        assign(dfname,df_state)

        fname<-paste0("./data/",year,"/brfss_",nm,"_",year,".RData")
        if(version>0) fname<-gsub("[.]RData",paste0("_V",version,".RData"),fname)

        save(list = c(dfname),file = fname)
        #browser()
        columns.add(year,add_cols)
      }
    },df_states$Id,df_states$Abbrev)
  })
  invisible()
}

columns.add<-function(year,cols2add){
  e<-new.env()
  fname<-paste0("./data/",year,"/columns_",year,".RData")
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
  loadfolder<-sub_pattern(loadfolder_pat,year)
  loadname<- sub_pattern(load_pat,year)
  if(version>0) loadname<-gsub("[.]RData",paste0("_V",version,".RData"),loadname)
  #browser()
  load(file = paste0(loadfolder,loadname),envir = e)
  get(ls(e),envir = e)
}


read.sas.format<-function(year,folder_pat=sas.folder.pattern.raw_data(),file_pat=sas.file.pattern.format()) {

  folder<-sub_pattern(folder_pat,year)
  file<-sub_pattern(file_pat,year)
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

read.sasout<-function(year,folder_pat=sas.folder.pattern.raw_data(),file_pat=sas.file.pattern.sasout()) {

  folder<-sub_pattern(folder_pat,year)
  file<-sub_pattern(file_pat,year)
  path<-paste0(folder,file)
  if(file.exists(path)){
    lines<-readLines(path)
    lines<-gsub("\f","",lines)

    lbl_line<-grep("^Label$" ,lines)
    lbl_lines<-(lbl_line+1):(max(grep("^;$" ,lines))-1)
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

    df<-dplyr::left_join(df_lbl,df_sections,by="varname")
    df$is_calculated<-grepl("Calculated Var",df$section)
    df$section_type<-""
    df$section_number<-""

    df$section_type[grep( "Section" ,df$section)]<- "Core"
    df$section_type[grep( "Module" ,df$section)]<- "Module"
    df$section_type[!grepl("[0-9]",df$section)]<-df$section[!grepl("[0-9]",df$section)]

    df$section_number<-as.integer(gsub("[^0-9]","",df$section))
    df$section_number[is.na(df$section_number)]<-""
    df$section_name<-gsub(".*:(.*)","\\1",df$section)

    #df<-df[order(df$section_type,df$section_number,df$index),]
  } else {
    df<-data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = F)
    colnames(df)<-c("varname","label","section","index",             #"range",
                    "is_calculated","section_type","section_number","section_name"  )
  }

  df[,c("varname","label","section_type","section_number","section_name","index","is_calculated")]
}




