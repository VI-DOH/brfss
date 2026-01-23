
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
#'
process_codebook <- function(progress = NULL) {

  params <- my.brfss.patterns()

  show_progress(progress,
                message = "Codebook ... saving layout")

  save_codebook_layout()

  show_progress(progress,
                message = "Codebook ... saving values ")

  save_codebook_values()
}

#' Download Codebook
#'
#' Download the CDC prepared codebook for the BRFSS data files. The format of the URL is not consistent by year
#' so more than one pattern is attempted and all should fail but one for a given year.
#'
#' @param fldrout character - folder to store codebook - NULL gets default
#' @param year integer - year of interest
#' @param progress progress object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_codebook(2020)
#' }
download_codebook <- function(fldrout = NULL, year = NULL, progress = NULL) {

  show_progress(progress,
                message = "Codebook ... downloading ")

  if(brfss.param(extent) != "public") {

    stop("extent must be 'public'. local codebooks are not downloaded from the CDC BRFSS website.")

  }

  ## get params from the my_brfss object, what are we working with at this time

  params <- my.brfss.patterns()

  ##  URL patterns are stored under the group  "codebook_downloads"

  pats<-names(get.pattern.group("codebook_downloads"))

  ## get storage location for the codebook
  ##    GEOG is not needed unless the user changes the folder pattern
  ##    but is included here in case

  if(!is.null(year)) brfss.param(year = year)

  if(is.null(fldrout)) fldrout <- apply.pattern("codebook_folder",params)

  fldrout <- gsub("([^[/])$","\\1/",fldrout)

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

         if(ext == ".zip") {
           dir <- dirname(destfile)
          unzip(destfile, exdir = dir)
          file.remove(destfile)
         }

        set.pattern("codebook_ext", gsub("[.]","",ext))
      }
    })
  )
}

codebook_file <- function() {

  exts <- c("pdf", "txt", "rtf", "html")

  params <- my.brfss.patterns()

  fldr <- apply.pattern("codebook_folder", params)

  files <- list.files(fldr)

  fil_ex <- files %>% gsub("[.].*","",.)

  fil <- apply.pattern("codebook_file", params)

  if(!fil %in% fil_ex) fil <- fil_ex[1]

  ret <- NULL

  found <- sapply(exts, function(ext) {
    file <- paste0(fldr,fil,".",ext)
    if(file.exists(file)) ret <<-  file
  })

  return(ret)

}

#############################################################
##
##    Determine if Codebook Exists
##
#' Find the CDC Provided Codebook File and return success or failure
#'
#' Uses the file_pattern data.frame
#'to figure out and try the try the possible names of the codebook file.
#'#'
#' @return logical was the file found
#' @export
#'
#'
codebook.exists <- function() {

  exts <- c("pdf", "txt", "rtf","html")

  params <- my.brfss.patterns()

  fldr <- apply.pattern("codebook_folder", params)
  fil <- apply.pattern("codebook_file", params)

  found <- sapply(exts, function(ext) {
    file <- paste0(fldr,fil,".",ext)

    file.exists(file)
  })

  return(any(found))

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


  args <- list(...)
  #
  #   params <- my.brfss.patterns()

  if(is.null(file)) {

    file <- codebook_file()
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

    lines <- readLines(file) %>%
      htm2txt::htm2txt(file) %>%
      gsub(" "," ",.) %>%
      grep("^$", ., invert = T, value = T) %>%
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

  lines %>% stringi::stri_trans_general("Latin-ASCII")

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

resection_codebook <- function(lines) {



  ## some codebooks (pdf) have Section and Label on same line
  ##   this splits them

  lntmp <- lines %>% gsub("(.*{5,}) (Section.Name.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Core.Section.Numb.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Module.Numb.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Question:.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Question Number:.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (SAS Variable Name:.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Type of Variable:.*)","\\1@@@\\2", .)
  lntmp <- lntmp %>% gsub("(.*{5,}) (Column:.*)","\\1@@@\\2", .)

  lines <- lntmp %>% stringr::str_split(., "@@@") %>% unlist()

  lines <- lines %>% gsub(" Question Prolo.*","", .)

  unlist(lines)

}

#############################################################
##
##    layout from codebook
##
save_codebook_layout <- function(file=NULL) {


  params <- my.brfss.patterns()

  lines <- read_codebook(file=file)
  if(is.null(lines)) return(NULL)

  ##############################################################
  ##
  ##  get variable values from "VARIABLE: value" lines

  ##    the "Label: xxxx..."  line is the start of the information for each eventual column of
  ##      BRFSS data
  ##    variables below ending in '_lines' are integer vectors with line numbers

  # ## some codebooks (pdf) have Section and Label on same line
  # ##   this splits them
  #
  # lntmp <- lines %>% gsub("(.*{5,}) (Section.Name.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Core.Section.Numb.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Module.Numb.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Question:.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Question Number:.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (SAS Variable Name:.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Type of Variable:.*)","\\1@@@\\2", .)
  # lntmp <- lntmp %>% gsub("(.*{5,}) (Column:.*)","\\1@@@\\2", .)
  #
  # lines <- lntmp %>% stringr::str_split(., "@@@") %>% unlist()
  #
  # lines <- lines %>% gsub(" Question Prolo.*","", .)
  #
  # lines <- unlist(lines)

  lines <- resection_codebook(lines)

  #============================================================

  label_lines <- grep("^Label:", lines)
  mod_lines <- grep("^Module.*Number", lines)
  core_lines <- grep("^Core.*Number", lines)
  sect_lines <- grep("^Sect.*Number", lines)
  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)
  ##
  columns <- grep("^Column:", lines, value = TRUE) %>% gsub(" Type of Variable.*", "", .)
  col_names <- grep("^SAS.", lines, value = TRUE)


  labels <- lines[label_lines]
  questions <- grep("^Question:", lines, value = TRUE)
  sections <- grep("^Section.Nam.*:", lines, value = TRUE)
  qnums <- grep("^Question.Number", lines, value = TRUE)
  var_types <- grep("^Type.*Variable:", lines, value = TRUE)

  label <- stringr::str_trim(gsub(".*?:(.*)","\\1",labels))
  question <- stringr::str_trim(gsub(".*?:(.*)","\\1",questions))

  section <- stringr::str_trim(gsub(".*?:(.*)","\\1",sections))
  question_num <- stringr::str_trim(gsub(".*:(.*)","\\1",qnums))
  var_type <- stringr::str_trim(gsub(".*?:(.*)","\\1",var_types))

  sect_type <- rep("",length(label_lines))
  sect_num <- rep("",length(label_lines))

  if(length(core_lines)== 0) {

    # didn't find phrase Core Section n ... eg. 2021 has it, but 2016 doesn't
    # 2016 ... Section n for both core and modules

    # get section lines values (parse the n)

    sects <- sapply(sect_lines, function(sect_ln) {
      as.integer(stringr::str_trim(gsub(".*?:(.*)","\\1",lines[sect_ln])))
    })

    #remove Section 0 lines

    sect_lines <- sect_lines[sects > 0]
    sects <- sects[sects > 0]

    sects_next <- c(sects[-1],999)

    splits <- which(sects_next <  sects)
    split <- splits[1] + 1
    end_mods <- splits[2]


    core_lines <- sect_lines[1:(split-1)]
    mod_lines <- sect_lines[split:end_mods]

  }
  ##    get Core Section numbers

  cors<-sapply(core_lines, function(core_ln) {

    max(which(label_lines<core_ln))
  })



  sect_type[cors] <- "Core"
  sect_num[cors] <- stringr::str_trim(gsub(".*?:(.*)","\\1",lines[core_lines]))


  ##    get Module Section numbers

  mods<-sapply(mod_lines, function(mod_ln) {

    max(which(label_lines<mod_ln))
  })

  sect_type[mods] <- "Module"
  sect_num[mods] <- stringr::str_trim(gsub(".*?:(.*)","\\1",lines[mod_lines]))

  ##   Blank Section Name for  Weighting Variables
  ##     for some reason, the state codebook for 2021 has
  ##      Module 1 for all weighting variables

  wgts <- grep("^Sect.*Nam.*:.*Weight", sections)
  sect_type[wgts] <- ""

  ##   set blank section types to the 'Non-Survey'

  calculated  <-  grepl("Calculated", section)
  blnks <- nchar(sect_type) == 0
  sect_type[blnks] <- "Non-Survey"

  ##
  ##    for some reason calculated variables are assigned to Modules instead of Core so we
  ##     have to change that

  sect_type[calculated] <- "Core"
  sect_num[blnks] <- 0

  ## parse column name and column range from text

  col_name <- stringr::str_trim(gsub(".*?:(.*)","\\1",col_names))
  column <- stringr::str_trim(gsub(".*?:(.*)","\\1",columns))


  ## build data frame for storage and later conversion to column name-column width pairs
  ##  for ASCII file reading

  df <- data.frame(column,col_name, label, question, section, sect_type,
                   sect_num, question_num, var_type, calculated) %>%
    mutate(start = as.integer(gsub("(.*)-(.*)","\\1",column))) %>%
    mutate(end = as.integer(gsub("(.*)-(.*)","\\2",column))) %>%
    mutate(field_size = end - start + 1) %>%
    relocate(sect_num, .after = sect_type) %>%
    select(-column)

  df_core <- df %>%
    filter(sect_type == "Core" & !calculated) %>%
    select(sect_type, sect_num, section) %>%
    distinct()


  df <- df %>% left_join(df_core, by = join_by(sect_type == sect_type, sect_num == sect_num)) %>%
    mutate(section = ifelse(calculated, section.y, section.x)) %>%
    select(-contains("."))

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
             question_num, var_type, question) %>%
    mutate(saq = FALSE)

  fldr <- apply.pattern("codebook_layout_folder",params)
  fil <- apply.pattern("codebook_layout_file", params)

  file <- paste0(fldr,fil)

  if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


  saveRDS(df_layout_cb, file = file)

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

  # fldr <- apply.pattern("codebook_layout_folder", params)
  # fil <- apply.pattern("codebook_layout_file", params)
  # file <- paste0(fldr,fil)

  file <- apply.pattern("codebook_layout_path", params)

  if(!file.exists(file)) {

    params["EXT"] <- "public"
    params["GFLAG"] <- "off"

    file <- apply.pattern("codebook_layout_path", params)

    if(!file.exists(file)) return(NULL)
  }

  readRDS(file = file)

}

#' Get Merged Layout
#'
#' Get layout created from merging public layout with state-added questions
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


  readRDS(file = file)

}


