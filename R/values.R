
require(dplyr)

split_it <- function(str, sep = "=") {
  pttrn <- paste0( "(.*)" , sep , "(.*)" )

  left <- stringr::str_trim(gsub(pttrn,"\\1",str))
  right <- stringr::str_trim(gsub(pttrn,"\\2",str))

  list(left=left,right=right)
}

#' Save Codebook Values
#'
#' The annual codebook is parsed for possible values for each variable and saved.
#'
#' @param file character - filename of layout file
#' @param year integer - year of interest for codebook
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @export
#'
#' @examples
save_codebook_values <- function(file = NULL) {

  params <- my.brfss.patterns()

  df_values_cb <- parse_codebook_values(file = file)

  fname <- apply.pattern("codebook_values_path",params)

  saveRDS(df_values_cb, file = fname)
}

stretch_values <- function(lines) {

  # skips <- get_skips()

  notes_lines <- grep("^Notes:",lines)

  lines <- lines[-notes_lines]

  label_lines <- grep("^Label:", lines)
  labels <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[label_lines]))
  col_name_lines <- grep("^SAS.", lines)
  col_names <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[col_name_lines]))

  ncols <- length(col_names)

  value_lines <- grep("^Value$",lines)

  # if the data are not in html
  if(length(value_lines) == 0 ) return(lines)


  wtd_lines <- grep("^Weighted.Perc",lines)
#  val_ends <- c(lbl_lines[-1]-1,length(lines))
  val_ends <- c(label_lines[-1]-1,length(lines))

  ##  build the new lines ... all values will be on a single line with their counts and percents
  ##    just like the pdf and rtf file
  ##

  new_lines <- character(0)

  next_inc <- 1
  as.vector(mapply(function(s,e) {

    new_lines <<- c(new_lines,lines[next_inc:(s-1)])
    ## get the lines for this value set
    lins_tmp <- lines[s:e]

    x<-sapply(seq(1,length(lins_tmp),5),function(ln) {
      paste0(lins_tmp[ln:(ln+4)],collapse = "     ")
    })
    new_lines <<- c(new_lines,x)

    next_inc <<- e+1

  },value_lines,val_ends)
  )

  new_lines
}

#' Parse Codebook Values
#'
#' The annual codebook is parsed for possible values for each variable.
#'
#' @param file character - filename of layout file
#' @param year integer - year of interest for codebook
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @return
#' @export
#'
#' @examples
parse_codebook_values <- function(file=NULL) {
    require(dplyr)

    lines <- read_codebook(file=file)
    if(is.null(lines)) return(NULL)

  #############################################################################

  value_lines <- grep(".*Value.*Value.Label", lines)
  if(length(value_lines) == 0) {
    lines <- stretch_values(lines)
    value_lines <- grep(".*Value.*Value.Label", lines)
  }

  label_lines <- grep("^Label:", lines)
  labels <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[label_lines]))
  col_name_lines <- grep("^SAS.", lines)
  col_names <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[col_name_lines]))

  ncols <- length(col_names)


  #################################################################
  ##
  ##  warn if the variables don't line up (different vector lengths)
  ##    perhaps from one or more badly formed variable lines
  ##
  ##      those lines should look something like this ...

  # Label: Told Had Arthritis
  # Section Name: Arthritis
  # Core Section Number: 8
  # Question Number: 1
  # Column: 132
  # Type of Variable: Num
  # SAS Variable Name: HAVARTH5
  # Question Prologue:
  # Question:  Has a doctor, nurse or other health professional ever told you that
  # Value    Value Label    Frequency    Percentage    Weighted Percentage


  if(length(col_name_lines) != length(value_lines)){
    warning("Number of col_names differs from number of value sections")
  }

     #######################################################################
  ##
  ##  remove the beginning junk

  rm_lines <- 1:(label_lines[1]-1)
  lines <- lines[-rm_lines]

  ## get rid of empty lines and lines with nothing in the first 10 characters

  lines <- grep("^$",lines, value = TRUE, invert = TRUE)
  lines <- grep("^[[:space:]]{10,}",lines, value = TRUE, invert = TRUE)

  # get the line numbers where the sas variable is and that variable value

  col_name_lines <- grep("^SAS.", lines)
  col_names <- split_it(lines[col_name_lines], ":")$right


  label_lines <- grep("^Label:", lines)
  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)


  col0 <- ""
  rm_lines <- integer(0)

  mapply(function(lbl_ln,val_ln, col_nm_ln,col) {
    if(col == col0) {

      rm_lines <<- c(rm_lines,lbl_ln:(val_ln+1))
    }

    rm_lines <<- c(rm_lines,lbl_ln:(col_nm_ln-1),(col_nm_ln+1):val_ln)

    col0 <<- col

  },label_lines, value_lines, col_name_lines, col_names)

  lines <- lines[-unique(rm_lines)]

  ##
  ##  get rid of lines starting with HIDDEN or BLANK
  ##    no useful information

  rm_lines <- grep("^ *HIDDEN|^ *BLANK",lines)

  lines <- lines[-rm_lines]


  col_name_lines <- grep("^SAS.", lines)
  col_names <- split_it(lines[col_name_lines], ":")$right

  value_lines <- grep("^[[:space:]]*Value[[:space:]]*Value Label", lines)


  lbl0 <- ""

  lbl <-sapply(1:length(lines),function(lin) {

    if(lin %in% col_name_lines) {
      lbl0 <<- split_it(lines[lin], ":")$right
    }
    lbl0
  })

  #browser()
  df <- data.frame(col_name = lbl, text = lines)

  df0 <- df %>%
    mutate(col_name = lbl)%>%
    filter(!grepl("(SAS.Var.*:|Value {6,}Value Lab)",.$text)) %>%
    filter(!grepl("^Notes:",.$text)) %>%
    mutate(text = stringr::str_trim(text))


  df_final <- df0 %>%
    mutate(text = gsub("(^.*?) {8,}.*","\\1",text))  %>%

    ## Get rid of 'go to somewhere' text in line
    mutate(text = gsub("[—]*Go to.*","",text))  %>%
    mutate(text = gsub("[—]*Code=.*","",text))  %>%
    mutate(text = gsub("If .* is .* go to.*","",text))  %>%


    mutate(value = gsub("^([[:digit:]]+).*","\\1",text)) %>%

    mutate(maxval = ifelse(grepl("^[[:digit:]]+ *- *([[:digit:]]+).*",text),
                           gsub("^[[:digit:]]+ *- *([[:digit:]]+).*","\\1",text), NA)) %>%
    mutate(text = gsub("^[[:digit:]]+ *- *([[:digit:]]+) +","",text))  %>%
    mutate(text = gsub("^[[:digit:]]+ *","",text))  %>%

    # special case IDAY ... no need for 31 lines ... make range 1 to 31
    filter(col_name != "IDAY" | value == "1") %>%
    mutate(maxval = ifelse(col_name == "IDAY","31", maxval)) %>%

    # special case REPDEPTH ... no need for 30 lines ... make range 1 to 30
    filter(col_name != "REPDEPTH" | value == "1") %>%
    mutate(maxval = ifelse(col_name == "REPDEPTH","30", maxval)) %>%

    #  get rid of numerics at end of each value name (totals that sometimes come over in codebook)
    #  Value	Value Label	           Frequency	Pct	Pct Wtd
    #      1 Yes	                       222	17.38	10.54
    #      2 No	                         919	71.97	76.71
    #      7 Don’t know/Not Sure	       136	10.65	12.75
    #  BLANK Not asked or Missing	       101    .     .

    mutate(text = gsub("(^.*?) {3,}.*","\\1",text))  %>%


    select(col_name,value, maxval, text)

  df_final

}
#' Get Values
#'
#' The annual codebook is parsed for possible values for each variable and saved. If SAQ Values are available,
#' they are merged. This function retrieves those values
#'
#' @param year integer - year of interest for codebook
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @export
#'
#' @examples
values <- function( year = NULL, ...) {

  params <- my.brfss.patterns()

  fname <- apply.pattern("merged_values_path",params)

  if(!file.exists(fname)) {
    fname <- apply.pattern("codebook_values_path",params)
  }
  readRDS(file = fname)
}

#' Get Codebook Values
#'
#' The annual codebook is parsed for possible values for each variable and saved. This function retrieves those values
#'
#' @param year integer - year of interest for codebook
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @export
#'
#' @examples
codebook_values <- function() {

  params <- my.brfss.patterns()

  fname <- apply.pattern("codebook_values_path",params)

  readRDS(file = fname)
}

#' Calculates the Type of BRFSS Questions
#'
#' This is normally not called by the user, but used internally to get the factors for each question.
#' Values returned are: factor, range, or NA. Weighting variables are among the NA values.
#'
#' @param df_layout data frame containng the column layout derived from the codebook
#' @param df_vals data frame containng the column values derived from the codebook
#' @param ... other arguments to be passed to apply.pattern()
#'
#' @return data frame continaing the type for each column
#' @export
#'
quest_types <- function(df_layout = NULL, df_vals = NULL, ...) {
  require(tibble, warn.conflicts = FALSE, quietly = TRUE)

  if(is.null(df_vals)) df_vals <- codebook_values(year = year, ...)

  if(is.null(df_layout)) df_layout <- get.codebook.layout(year = year, ...)

  is.calc <- grepl("^Calc",df_layout$section)
  is.wt <- grepl("[Ww]eighting.V",df_layout$section)

  col_names <- df_layout %>% pull(col_name)
  #  df <- data.frame(col_name = col_names)

  types <- mapply(function(col, calc, wt) {

    df_lo <- df_layout %>% filter(col_name == col)

    df0 <- df_vals %>%
      filter(col_name == col)

    if(nrow(df0)>0) {
      maxv <- df0 %>% pull(maxval)
      txt <- df0 %>% pull(text)

      dk <- any(grepl("^[Dd]on.{0,1}t [Kk]now",txt))
      ref <-any(grepl("^[Rr]efused$",txt))
      yes1 <- tolower(txt[1]) == "yes"
      fm1 <- grepl("male",tolower(txt[1]))

      #if(col == "DIFFWALK") browser()

      #      if(any(!is.na(maxv)) && !calc && !wt) {
      if(any(!is.na(maxv)) && !wt) {
        return("range")
        #      } else if((fm1 || dk || ref || yes1) && !wt){ #&& !calc && !wt){
      } else if(!wt){ #&& !calc && !wt){
        return ("factor")
      }else {
        return ("unknown")
      }
    } else {
      return("unknown")
    }
  }, col_names, is.calc, is.wt)

  types[grepl("_STSTR",col_names)] <- "stratum"

  df <- data.frame(col_name = col_names, type = types,
                   calc = is.calc, wt = is.wt,
                   row.names = NULL)
  df
}

#' Convert BRFSS Columns to Factor
#'
#' Use values file (created from codebook) to convert applicable BRFSS columns to factors
#'
#' @param df_brfss data.frame BRFSS data file
#' @param year integer - year of interest
#' @param df_layout - layout data for the year
#' @param df_vals  - value data for the year
#'
#' @return data.frame of BRFSS data with factored columns
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 2020
#' df_brfss <- brfss_data(year = year, geog = "MT")
#' df_layout <- get.codebook.layout(year = year)
#' df_vals <- codebook_values(year = year)
#' df_brfss <- make_factors(df_brfss = df_brfss, df_layout = df_layout, df_vals = df_vals)
#' }
#'
make_factors <- function(df_brfss = NULL, df_layout = NULL, df_vals = NULL) {

  if(is.null(df_brfss)) df_brfss <- brfss_data()
  if(is.null(df_vals)) df_vals <- values()

  if(is.null(df_layout)) df_layout <- get.layout()

  browser()
  df_factors <- quest_types(df_layout, df_vals) %>%
    filter(type == "factor")  %>%
    {row.names(.)<-NULL;.}

  invisible(
    sapply(df_factors$col_name, function(col) {

      a<- attributes(df_brfss[[col]])
      df <- df_vals %>% filter(col_name==col)
      levels <- df %>% pull(value)
      labels <- df %>% pull(text)
      x <- df_brfss[[col]]

      if(orrr::is.integer_like(levels)) levels <- as.integer(levels)
      if(orrr::is.integer_like(x)) x <- as.integer(x)

      f <- factor(x,levels = levels, labels = labels)
      attributes(f) <- c(attributes(f),a)
      df_brfss[col] <<- f
    })
  )

  df_brfss
}


#' Subset Data For Valid Responses
#'
#' Removes 'Don't Know' and 'Refused' as well as Missing responses
#'
#' Invalid responses are indicated by the `invalid` pattern. Include other columns,
#' such as weighting and others for subsetting, like sex or age.
#'
#' The default pattern for invalid responses is "don.{0,1}t know|refused" (regexp formatted)
#'
#' @param df data frame - BRFSS data
#' @param coi  character - column to use for filtering
#' @param other_cols - other columns to keep
#' @param invalid - pattern for invalid rsponses
#'
#' @return the subsetted data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- brfss_data()
#' coi <- "GENHLTH"
#' other_cols <- c("_LLCPWT","_SEX","_AGE65YR")
#' df_coi <- df %>% valid_only(coi = coi, other_cols = other_cols)
#' }
#'
valid_only <- function(df, coi, other_cols=character(0), invalid = c("don.{0,1}t know|refused")) {

  df <- df %>%
    select({{coi}}, other_cols) %>%
    filter(!grepl(invalid,.[[coi]], ignore.case = TRUE)) %>%
    filter(!is.na(.[[coi]]))

  #  get rid of empty levels

  if(is.factor(df[[coi]])) df <- df %>%
      mutate({{coi}} := droplevels(.[[coi]]))

  df
}

#' Simple Response Counts
#'
#' Report counts of responses in a simply formatted table. Options include removing NAs, including only valid responses,
#' including totals, and including percents. Table is printed to console.
#'
#' @param df_brfss data frame - BRFSS data
#' @param coi character or symbol - the column to report on
#' @param na.rm logical - remove NAs (default = TRUE)
#' @param valid logical - remove "Don't Know" and "Refused" responses (default = FALSE)
#' @param total logical - show a total line (default = TRUE)
#' @param pct logical - include a percent column (default = TRUE)
#'

#' @export
#'
#' @examples
#' \dontrun{
#' df_brfss <- brfss_data(year = 2021, geog = "VI")
#' df_brfss %>% simple_counts(COVIDVAC, valid = T)
#' }
simple_counts <- function(df_brfss, coi, na.rm = TRUE, valid = FALSE, total = TRUE, pct = TRUE) {

  arguments <- as.list(match.call())
  coi = eval(arguments$coi, df_brfss)

  if(length(coi) > 1 || !coi %in% colnames(df_brfss)) coi <- rlang::as_name(arguments$coi)

  if(na.rm) df_brfss <- df_brfss %>% filter(!is.na(!!sym(coi)))

  if(valid) df_brfss <- df_brfss %>% valid_only(coi = coi)


  df_totals <- df_brfss %>%
    group_by(!!sym(coi)) %>%
    summarise(n=n()) %>%
    as.data.frame() %>%
    mutate(!!sym(coi) := as.character(!!sym(coi)))

  if(pct) df_totals <- df_totals %>% mutate(pct = round(n/sum(n)*100,2))


  if(total) {
    df_totals <- df_totals %>%
      bind_rows(
        {
          x<-data.frame(XXX="TOTAL",n=sum(.$n));
          if(total) x <- x %>% mutate(pct = 100.00)

          colnames(x)[1] <- coi;
          x
        }
      )
  }

  # section_type<-att["section_type"]
  # section_num<-att["section_num"]
  # section_index<-att["section_index"]
  # section_name<-att["section_name"]
  # label<-att["label"]

  title <- paste0(attributes(df_brfss[[coi]])$section_type, " ",
                 attributes(df_brfss[[coi]])$section_num, ": ",
                 attributes(df_brfss[[coi]])$section_name)

  title <- c(title,attributes(df_brfss[[coi]])$label)

  x <- capture.output(print(df_totals, row.names=F))

  tblwid <- nchar(x[1])
  line1 <- strrep("=",max(nchar(title)))
  line2 <- strrep("-",tblwid)
  line3 <- strrep("-",tblwid)

  x<-c("\n",title,line1,x[1],line2,x[2:(length(x)-1)],line3,tail(x,1))
  x <- paste0(x,collapse = "\n")
  cat(x)
}
