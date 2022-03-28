

#############################################################
##
##    layout from codebook
##
#' Build the Layout from CDC Provided Codebook
#'
#' If a filename is not provided, it uses the file_pattern data.frame
#'to figure out the name of the file. The layout data.frame is used to
#'parse the BRFSS ascii fixed width file
#'
#' @param file character name of layout file - if missing (NULL) then my.brfss() is called to
#' determine the year and geographical place of interest. The geographical place of interest
#' may not be used, but will be passed for pattern matching of file name in case it is needed
#'
#' @return data.frame containing the layout to be passed to read.fwf
#' @export
#'
#' @examples
#############################################################
##
##    layout from codebook
##
codebook_layout <- function(file=NULL) {
  require(dplyr)

  if(is.null(file)) {
    x <- my.brfss()
    fldr <- apply.pattern("codebook_layout_folder", YEAR = x$year, GEOG = x$geog)
    fil <- apply.pattern("codebook_layout_file", YEAR = x$year, GEOG = x$geog)
    ext <- apply.pattern("codebook_layout_foldext", YEAR = x$year, GEOG = x$geog)
    file <- paste0(fldr,fil,ext)
  }

  if(grepl("[.]txt$", file, ignore.case = TRUE)) {

    lines <- readLines(file)

  } else if(grepl("[.]rtf$", file,ignore.case = TRUE)) {

    lines <- striprtf::read_rtf(file)

    lines <- gsub("^[*]| {0,1}","",lines)
    lines <- lines %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else if(grepl("[.]pdf$", file,ignore.case = TRUE)) {

    lines <- pdftools::pdf_text(file) %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else {
    return(NULL)
  }


  label_lines <- grep("^Label:", lines)
  question_lines <- grep("^Question:", lines)
  mod_lines <- grep("^Module.*Number", lines)
  core_lines <- grep("^Core.*Number", lines)

  labels <- grep("^Label:", lines, value = TRUE)
  questions <- grep("^Question:", lines, value = TRUE)
  sections <- grep("^Section Name:", lines, value = TRUE)
  qnums <- grep("^Question Number", lines, value = TRUE)

  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)
  cols_lines <- grep("^Column:", lines, value = TRUE)
  var_lines <- grep("^SAS.", lines, value = TRUE)

  label <- stringr::str_trim(gsub(".*:(.*)","\\1",labels))
  question <- stringr::str_trim(gsub(".*:(.*)","\\1",questions))
  section <- stringr::str_trim(gsub(".*:(.*)","\\1",sections))
  question_num <- stringr::str_trim(gsub(".*:(.*)","\\1",qnums))

  sect_type <- rep("",length(label_lines))
  sect_num <- rep("",length(label_lines))

  cors<-sapply(core_lines, function(core_ln) {

    max(which(label_lines<core_ln))
  })

  sect_type[cors] <- "Core"
  sect_num[cors] <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[core_lines]))

  mods<-sapply(mod_lines, function(mod_ln) {

    max(which(label_lines<mod_ln))
  })

  sect_type[mods] <- "Module"
  sect_num[mods] <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[mod_lines]))


  col_name <- stringr::str_trim(gsub(".*:(.*)","\\1",var_lines))
  col_name <- stringr::str_trim(gsub(".*:(.*)","\\1",var_lines))
  column <- stringr::str_trim(gsub(".*:(.*)","\\1",cols_lines))

  df <- data.frame(column,col_name, label, question, section, sect_type, sect_num) %>%
    mutate(start = as.integer(gsub("(.*)-(.*)","\\1",column))) %>%
    mutate(end = as.integer(gsub("(.*)-(.*)","\\2",column))) %>%
    mutate(field_size = end - start + 1)

  dups <- which(duplicated(df$start))

  rms <- integer(0)

  invisible(
    sapply(dups, function(dup) {
      end1 <- df[dup,"end"]
      end0 <- df[dup-1,"end"]
      if(end0>=end1) rm <- dup-1 else rm <- dup
      rms <<- c(rms,rm)
    })
  )

  df <- df %>%
    slice(-rms)

  df <- df  %>%
    bind_rows(df  %>%
                mutate(exp = c(0,end[1:(nrow(.)-1)])+1) %>%
                filter(exp != start) %>%
                mutate(field_size = start - exp ) %>%
                mutate(start = exp) %>%
                mutate(end = start + field_size - 1) %>%
                mutate(col_name = paste0("DUMMY_" , start)) %>%
                #        mutate(field_size = -field_size) %>%
                select(col_name, start, end, field_size, label, question, section, sect_type, sect_num)
    ) %>%
    arrange(start)

  if(dir.exists("./output"))   write.csv(x = df  ,file = "./output/codebook_layout.csv")

  df %>%
    select(field_size, col_name)

}

codebook_layout_SAVE <- function(file=NULL) {
  require(dplyr)

  if(is.null(file)) {
    x <- my.brfss()
    fldr <- apply.pattern("codebook_layout_folder", YEAR = x$year, GEOG = x$geog)
    fil <- apply.pattern("codebook_layout_file", YEAR = x$year, GEOG = x$geog)
    ext <- apply.pattern("codebook_layout_foldext", YEAR = x$year, GEOG = x$geog)
    file <- paste0(fldr,fil,ext)
  }

  if(grepl("[.]txt$", file, ignore.case = TRUE)) {

    lines <- readLines(file)

  } else if(grepl("[.]rtf$", file,ignore.case = TRUE)) {

    lines <- striprtf::read_rtf(file)

    lines <- gsub("^[*]| {0,1}","",lines)
    lines <- lines %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else if(grepl("[.]pdf$", file,ignore.case = TRUE)) {

    lines <- pdftools::pdf_text(file) %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else {
    return(NULL)
  }
  browser()

  label_lines <- grep("^Column:", lines)
  question_lines <- grep("^Question:", lines)
  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)
  cols_lines <- grep("^Column:", lines, value = TRUE)
  var_lines <- grep("^SAS.", lines, value = TRUE)

  col_name <- stringr::str_trim(gsub(".*:(.*)","\\1",var_lines))
  column <- stringr::str_trim(gsub(".*:(.*)","\\1",cols_lines))

  df <- data.frame(column,col_name) %>%
    mutate(start = as.integer(gsub("(.*)-(.*)","\\1",column))) %>%
    mutate(end = as.integer(gsub("(.*)-(.*)","\\2",column))) %>%
    mutate(field_size = end - start + 1)

  dups <- which(duplicated(df$start))

  rms <- integer(0)

  invisible(
    sapply(dups, function(dup) {
      end1 <- df[dup,"end"]
      end0 <- df[dup-1,"end"]
      if(end0>=end1) rm <- dup-1 else rm <- dup
      rms <<- c(rms,rm)
    })
  )

  df <- df %>%
    slice(-rms)

  df <- df  %>%
    bind_rows(df  %>%
                mutate(exp = c(0,end[1:(nrow(.)-1)])+1) %>%
                filter(exp != start) %>%
                mutate(field_size = start - exp ) %>%
                mutate(start = exp) %>%
                mutate(end = start + field_size - 1) %>%
                mutate(col_name = paste0("DUMMY_" , start)) %>%
                #        mutate(field_size = -field_size) %>%
                select(col_name, start, end, field_size)
    ) %>%
    arrange(start)

  if(dir.exists("./output"))   write.csv(x = df  ,file = "./output/codebook_layout.csv")

  df %>%
    select(field_size, col_name)

}


sas_layout<-function(year){

  file= paste0(orrr::dir.project("data_raw"),year,"/SASOUT",year%%100,"_STATES.SAS")
  read.sas.layout(year = year,file = file)
}


fixed_width_layout<-function(year) {

  df_saq<- saq_data(year)
  # get sas layout provided by CDC

  df_sas_layout<-sas_layout(year)

  # clean up sas layout - remove overlapping vars
  #   and remove the state-added-question columns which are to be added from df_saq

  df_layout<-df_sas_layout[!df_sas_layout$col_name%in%c('IDATE','X_PSU',"STATEQUE"),]

  df_layout<-rbind(df_layout[,c("start","end","col_name","field_size","description")],
                   df_saq[,c("start","end","col_name","field_size","description")])

  # build ordered vectors for start and end with no gaps
  #   and compute widths for read.fwf
  #   gaps are replaced with -widths to ignore those columns

  df_layout$start<-as.integer(df_layout$start)
  df_layout$end<-as.integer(df_layout$end)

  df_layout<-df_layout[order(df_layout$start),]

  st<-df_layout$start
  end<-df_layout$end

  # calculate nxt ... the starts of the following items
  nxt<-c(st[-1],st[length(st)]+1)

  # here is where we fix the gaps ...
  #  if there is not data in the column(s) following those data, then we add a gap
  #   fix is a vector of items that lack data in the next column,
  #   i.e. the next data starts in column x which is not 1 column after the end of my data

  fix<-which(nxt!=(end+1))
  add_st<-end[fix]+1
  add_end<-st[fix+1]-1

  # combine the columns data with the added gaps and reorder by start column

  df_fwf<-rbind(df_layout[,c("start","end","field_size")],   data.frame(start=add_st,end=add_end,field_size=-(add_end-add_st+1)))

  df_fwf<-df_fwf[order(df_fwf$start),]
  list(widths=df_fwf$field_size,cols=df_layout$col_name,layout=df_layout)
}


merge_layout<-function(df_quest) {

  df_layout<-sas_layout(2021)

  df_quest<- df_quest %>%
    mutate(index = as.integer(qnumber)) %>%
    rename(type_index = snumber)

  df_questions<-dplyr::left_join(df_quest,df_layout) %>%
    select(qnum,question,type,section,type_index,code,index,description,col_name)

}
