
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
#'
#' @export
#'
#' @examples
save_codebook_values <- function(file = NULL, year = NULL) {

  df_values_cb <- codebook_values(file = file, year = year)

  fname <- apply.pattern("codebook_values_path", YEAR=year)

  save(df_values_cb, file = fname)
}

#' Codebook Values
#'
#' The annual codebook is parsed for possible values for each variable.
#'
#' @param file character - filename of layout file
#' @param year integer - year of interest for codebook
#'
#' @return
#' @export
#'
#' @examples
codebook_values <- function(file = NULL, year = NULL) {

  if (is.null(file)) {
    year <- get.year(year)
    lines <- brfss::read_codebook(year = year)
  } else {
    lines <- brfss::read_codebook(file = file)

  }

  #############################################################################

  label_lines <- grep("^Label:", lines)
  labels <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[label_lines]))
  col_name_lines <- grep("^SAS.", lines)
  col_names <- stringr::str_trim(gsub(".*:(.*)","\\1",lines[col_name_lines]))

  ncols <- length(col_names)

  value_lines <- grep(".*Value.*Value.Label", lines)

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
    mutate(text = gsub("[—]*Go to .*","",text))  %>%
    mutate(text = gsub("If .* is .* go to.*","",text))  %>%
    #    mutate(text = gsub("(^.*)—Go to .*","\\1",text)) %>%

    mutate(value = gsub("^([[:digit:]]+).*","\\1",text)) %>%
    #mutate(value = ifelse(grepl("^HIDDEN",text),NA,value)) %>%
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

quest_types <- function(df_layout, df_vals) {


  is.calc <- grepl("^Calc",df_layout$section)
  is.wt <- grepl("[Ww]eighting.V",df_layout$section)

  #browser()
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

      #if(col == "DIFFWALK") browser()

      if(any(!is.na(maxv)) && !calc && !wt) {
        return("range")
      } else if((dk || ref || yes1) && !calc && !wt){
        return ("factor")
      }else {
        return (NA)
      }
    } else {
      return(NA)
    }
  }, col_names, is.calc, is.wt)

  df <- data.frame(col_name = col_names, type = types, calc = is.calc, wt = is.wt)
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
#' df_brfss <- factorize(df_brfss = df_brfss, df_layout = df_layout, df_vals = df_vals)
#' }
#'
factorize <- function(df_brfss = NULL, year = NULL, df_layout = NULL, df_vals = NULL) {

  #year = get.year(year)

  if(is.null(df_brfss)) df_brfss <- brfss_data()
  if(is.null(df_vals)) df_vals <- codebook_values(year = year)

  if(is.null(df_layout)) df_layout <- get.codebook.layout(year = year)


  df_factors <- quest_types(df_layout, df_vals) %>%
    filter(type == "factor")  %>%
    {row.names(.)<-NULL;.}

  df_brfss <- brfss::brfss_data()

  invisible(
    sapply(df_factors$col_name, function(col) {
      a<- attributes(df_brfss[[col]])
      df <- df_vals %>% filter(col_name==col)
      levels <- df %>% pull(value)
      labels <- df %>% pull(text)
      x <- factor(df_brfss[[col]],levels = levels, labels = labels)
      attributes(x) <- c(attributes(x),a)
      df_brfss[col] <<- x
    })
  )

  df_brfss
}

valid_only <- function(data, invalid = c("don.{0,1}t know)|refused")) {

  data %>% filter(!grepl(invalid,., ignore.case = TRUE)) %>%
    filter(!is.na(.))



}
