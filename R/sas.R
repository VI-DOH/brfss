

read.xpt<-function(year,readfolder_pat="./data_raw/[YEAR]/sas/",
                   file_pat="LLCP[YEAR].XPT",
                   savefolder_pat="./data/[YEAR]/",
                   save_pat="xpt_[YEAR].RData") {

  folder<-sub_year(readfolder_pat,year)
  file<-sub_year(file_pat,year)
  savefolder<-sub_year(savefolder_pat,year)
  savename<- sub_year(save_pat,year)

  df_xpt<- haven::read_xpt(paste0(folder,file))

  save(df_xpt,file = paste0(savefolder,savename))
}

sub_year<-function(strIn, year) {
  ret<-gsub("[YEAR]",year,strIn,fixed = T)
  gsub("[YR]",year%%100,ret,fixed = T)
}

read.sas.format<-function(year,folder_pat="./data_raw/[YEAR]/sas/",file_pat="Format[YR].sas") {

  folder<-sub_year(folder_pat,year)
  file<-sub_year(file_pat,year)
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

read.sasout<-function(year,folder_pat="./data_raw/[YEAR]/sas/",file_pat="SASOUT[YR]_LLCP.sas") {

  folder<-sub_year(folder_pat,year)
  file<-sub_year(file_pat,year)
  paste0(folder,file)

  lines<-readLines(paste0(folder,file))

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
  sects<-stringr::str_trim(gsub(".*/[*](.*)[*]/","\\1",input_lines[sect_lines]))

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

  df_sections<-data.frame(section=sections,index=indices,varname=varnames,range=ranges,stringsAsFactors = F)

  df<-dplyr::left_join(df_lbl,df_sections,by="varname")

  df$is_calculated<-grepl("Calculated Var",df$section)
  df$section_type<-""
  df$section_number<-""
  df$section_type[grep("Cell Phone Introduction",df$section)]<-"Cell Phone Introduction"
  df$section_type[grep( "Land Line Introduction" ,df$section)]<- "Land Line Introduction"
  df$section_type[grep( "Record Identification" ,df$section)]<- "Record Identification"
  df$section_type[grep( "Section" ,df$section)]<- "Core"
  df$section_type[grep( "Module" ,df$section)]<- "Module"
  df$section_type[!grepl("[0-9]",df$section)]<-df$section[!grepl("[0-9]",df$section)]

  df$section_number<-as.integer(gsub(".*([0-9]+).*","\\1",df$section))
  df$section_number[is.na(df$section_number)]<-""
  df$section_name<-gsub(".*:(.*)","\\1",df$section)

  df[order(df$section_type,df$section_number,df$index),]
}


