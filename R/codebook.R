
#' Download Codebook and Extract the Layout
#'
#' Download the CDC prepared codebook for the BRFSS data files and extract the layout. The format of the URL is not consistent by year so more than one pattern is attempted and all should fail but one for a given year.
#'
#' @param year integer - year of interest
#'
#' @export
#'
#' @examples
#' \dontrun{
#' process_codebook(2020)
#' }
process_codebook <- function() {

  params <- my.brfss.patterns()

  if(brfss.param(extent) == "national") download_codebook()

  save_codebook_layout()
  save_codebook_values()
}

#' Download Codebook
#'
#' Download the CDC prepared codebook for the BRFSS data files. The format of the URL is not consistent by year
#' so more than one pattern is attempted and all should fail but one for a given year.
#'
#' @param year integer - year of interest
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_codebook(2020)
#' }
download_codebook <- function() {

  ## if year is not provided then get the year from the my_brfss object
  params <- my.brfss.patterns()

  ##  URL patterns are stored under the group  "codebook_downloads"

  pats<-names(brfss::get.pattern.group("codebook_downloads"))

  ## get storage location for the codebook
  ##    GEOG is not needed unless the user changes the folder pattern
  ##    but is included here in case

  fldrout <- apply.pattern("codebook_folder",params)
  ext <- apply.pattern("codebook_ext",params)

  ## create the folder/dir if it does not exist

  if(!dir.exists(fldrout)) dir.create(fldrout, recursive = TRUE)

  ## for each pattern possibility (only one will succeed)
  ##  still looking for a clean way to suppress the fails but
  ##    report on the success


  invisible(
    sapply(pats,function(pat) {

      url <- apply.pattern(pat, params)

      ext <- gsub(".*([.].*)", "\\1", url)

      fileout <- apply.pattern("codebook_file",params)
      fileout <- paste0(fileout,ext)
      destfile <- paste0(fldrout,fileout)

      status <- httr::HEAD(url)$status
      if(status==200) {

        to <- getOption("timeout")
        options(timeout = 180)

        download.file(url = url,destfile = destfile,
                      method = "libcurl",quiet = F, mode = "wb")

        options(timeout = to)

        set.pattern("codebook_ext", gsub("[.]","",ext))
      }
    })
  )
}
#############################################################
##
##    Read Lines from Codebook
##
#' Read the CDC Provided Codebook File
#'
#' If a filename is not provided, it uses the file_pattern data.frame
#'to figure out the name of the file. The layout data.frame is used to
#'parse the BRFSS ascii fixed width file, and to populate column attributes with information such
#'as label, survey section module number. If a file name is not passed, then my.brfss() is called to
#' determine the year and geographical place of interest. The geographical place of interest
#' may not be used, but will be passed for pattern matching of file name in case it is needed
#'
#' @param file character name of layout file
#'
#' @return character vector containing the lines in the file
#' @export
#'
#'
read_codebook <- function(file=NULL, ...) {
  require(dplyr)

  args <- list(...)

  params <- my.brfss.patterns()

  if(is.null(file)) {
    fldr <- apply.pattern("codebook_folder", params)
    fil <- apply.pattern("codebook_file", params)
    ext <- apply.pattern("codebook_ext", params)
    file <- paste0(fldr,fil,".",ext)
  }

  if(!file.exists(file)) {
    message(paste0("File not found ... \n",file))
    return(NULL)
  }

  if(grepl("[.]txt$", file, ignore.case = TRUE)) {

    lines <- readLines(file)

  } else if(grepl("[.]rtf$", file,ignore.case = TRUE)) {

    lines <- striprtf::read_rtf(file)

    lines <- gsub("^[*][|] {0,1}","",lines)
    lines <- gsub("[|]","  ",lines)

    lines <- lines %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

    lines <- gsub("^[|]","",lines)

  } else if(grepl("[.]pdf$", file,ignore.case = TRUE)) {

    lines <- pdftools::pdf_text(file) %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else if(grepl("[.]html$", file,ignore.case = TRUE)) {

    lines <- readLines(file)
    lines <- lines %>%
      htm2txt::htm2txt(file)

    lines <- lines %>%
      gsub(" "," ",.)

    lines <- lines %>%
      grep("^$", ., invert = T, value = T)

    lines <- lines %>%
      grep("^ *$", ., invert = T, value = T)

    comments <- rev(grep("<!--",lines))
    comment_ends <- rev(grep("-->",lines))

    mapply(function(s,e) {
      lines <<- lines[-(s:e)]
    }, comments, comment_ends)

    lines <- lines %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE)

  } else {
    lines <- NULL
  }

  lines
}

#' Set Codebook Extension
#'
#' Convenience function to set the codebook extension
#'
#' @param ext character - extension (pdf, rtf, txt, or html)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_codebook_ext("pdf")
#' }
#'
set_codebook_ext <- function(ext) {

  set.pattern(name = "codebook_ext", pattern = ext )
}

#############################################################
##
##    Save Layout from codebook
##
#' Build the Layout from CDC Provided Codebook
#'
#' If a filename is not provided, it uses the file_pattern data.frame
#'to figure out the name of the file. The layout data.frame is used to
#'parse the BRFSS ascii fixed width file, and to populate column attributes with information such
#'as label, survey section module number. If a file name is not passed, then my.brfss() is called to
#' determine the year and geographical place of interest. The geographical place of interest
#' may not be used, but will be passed for pattern matching of file name in case it is needed
#'
#' @param file character name of layout file
#'
#' @return data.frame containing the layout to be used for decoding/parsing the ascii file
#' @export
#'
#'

#############################################################
##
##    layout from codebook
##
save_codebook_layout <- function(file=NULL) {
  require(dplyr)

  params <- my.brfss.patterns()

  lines <- read_codebook(file=file)
  if(is.null(lines)) return(NULL)

  ##############################################################
  ##
  ##  get variable values from "VARIABLE: value" lines

  ##    the "Label: xxxx..."  line is the start of the information for each eventual column of
  ##      BRFSS data
  ##    variables below ending in '_lines' are integer vectors with line numbers

  label_lines <- grep("^Label:", lines)
  mod_lines <- grep("^Module.*Number", lines)
  core_lines <- grep("^Core.*Number", lines)
  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)

  ##
  columns <- grep("^Column:", lines, value = TRUE)
  col_names <- grep("^SAS.", lines, value = TRUE)


  labels <- lines[label_lines]
  questions <- grep("^Question:", lines, value = TRUE)
  sections <- grep("^Section.Nam.*:", lines, value = TRUE)
  qnums <- grep("^Question.Number", lines, value = TRUE)
  var_types <- grep("^Type.*Variable:", lines, value = TRUE)

  label <- stringr::str_trim(gsub(".*:(.*)","\\1",labels))
  question <- stringr::str_trim(gsub(".*:(.*)","\\1",questions))
  section <- stringr::str_trim(gsub(".*:(.*)","\\1",sections))
  question_num <- stringr::str_trim(gsub(".*:(.*)","\\1",qnums))
  var_type <- stringr::str_trim(gsub(".*:(.*)","\\1",var_types))

  sect_type <- rep("",length(label_lines))
  sect_num <- rep("",length(label_lines))

  ##    get Core Section numbers

  cors<-sapply(core_lines, function(core_ln) {

    max(which(label_lines<core_ln))
  })

  sect_type[cors] <- "Core"
  sect_num[cors] <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[core_lines]))

  ##    get Module Section numbers

  mods<-sapply(mod_lines, function(mod_ln) {

    max(which(label_lines<mod_ln))
  })

  sect_type[mods] <- "Module"
  sect_num[mods] <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[mod_lines]))

  ##   Blank Section Name for  Weighting Variables
  ##     for some reason, the state codebook for 2021 has
  ##      Module 1 for all weighting variables

  wgts <- grep("^Sect.*Nam.*:.*Weight", sections)
  sect_type[wgts] <- ""

  ##   set blank section types to the 'Non-Survey'

  blnks <- nchar(sect_type) == 0
  sect_type[blnks] <- "Non-Survey"
  sect_num[blnks] <- 0

  ## parse column name and column range from text

  col_name <- stringr::str_trim(gsub(".*:(.*)","\\1",col_names))
  column <- stringr::str_trim(gsub(".*:(.*)","\\1",columns))

  ## build data frame for storage and later conversion to column name-column width pairs
  ##  for ASCII file reading

  df <- data.frame(column,col_name, label, question, section, sect_type, sect_num, question_num, var_type) %>%
    mutate(start = as.integer(gsub("(.*)-(.*)","\\1",column))) %>%
    mutate(end = as.integer(gsub("(.*)-(.*)","\\2",column))) %>%
    mutate(field_size = end - start + 1) %>%
    relocate(sect_num, .after = sect_type) %>%
    select(-column)

  ##
  ## remove duplicates (overlaps of columns)
  ##
  ## e.g. IDATE (8 chars) always overlaps IYEAR, IMONTH and IDAY
  ##  and _PSU is always co-located with SEQNO
  ##  not sure of the history on the latter one

  df <- df %>% deduped_layout()

  ## add DUMMY variables for missing columns (non-contiguous end of one and start of the next)
  ##    if Var1 starts at column 50 and ends at column 52 and the next variable starts at 60 then
  ##    we have to have a DUUMMY variable from 53-59

  df_layout_cb <- df  %>% fill_dummies() %>%
    mutate(var_type = ifelse(is.na(var_type),"character",var_type)) %>%
    mutate(var_type = ifelse(var_type == "Num","integer",var_type)) %>%
    mutate(var_type = ifelse(var_type == "Char","character",var_type)) %>%
    relocate(field_size, start, end, col_name, sect_type, sect_num, section, label,
           question_num, var_type, question)

  fldr <- apply.pattern("codebook_layout_folder",params)
  fil <- apply.pattern("codebook_layout_file", params)

  file <- paste0(fldr,fil)

  if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


  save(df_layout_cb, file = file)

  invisible()

}

##
## remove duplicates (overlaps of columns)
##
## e.g. IDATE (8 chars) always overlaps IYEAR, IMONTH and IDAY
##  and _PSU is always co-located with SEQNO
##  not sure of the history on the latter one
##
## remove duplicates (overlaps of columns)
##
## e.g. IDATE (8 chars) always overlaps IYEAR, IMONTH and IDAY
##  and _PSU is always co-located with SEQNO
##  not sure of the history on the latter one
##
## read.fwf (read fixed width file) does not allow for overlapping columns
##  as it requires only a vector of columns widths, not start/stop

deduped_layout <- function(df) {
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

  ## remove duplicate rows

  df <- df %>%
    slice(-rms)

  df

}

#' Get layout created from the codebook
#'
#' @param year integer - year of interest
#'
#' @return data frame with layout data
#' @export
#'

get.codebook.layout <- function() {

  params <- my.brfss.patterns()

  fldr <- apply.pattern("codebook_layout_folder", params)
  fil <- apply.pattern("codebook_layout_file", params)

  file <- paste0(fldr,fil)

  if(!file.exists(file)) return(NULL)


  orrr::get.rdata(file = file)

}

#' Get Merged Layout
#'
#' Get layout created from merging national layout with state-added questions
#'
#' @param year integer - year of interest
#'
#' @return data frame with layout data
#' @export
#'

get.merged.layout <- function() {

  params <- my.brfss.patterns()

  fldr <- apply.pattern("codebook_layout_folder", params)
  fil <- apply.pattern("merged_layout_file",params)

  file <- paste0(fldr,fil)

  if(!file.exists(file)) return(NULL)


  orrr::get.rdata(file = file)

}


