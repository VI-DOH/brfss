

######################################################################################
##
##    excel version of wide table

#' Stats Table Across - Excel
#'
#' Produces a table similar to gt that lists the question values across the
#' top of the table and the compared variables and subsets vertically on the left
#'
#' @param df_stats data frame - produced by a call to survey_stats
#' @param file character - excel file name (path)
#' @param stats character stats of interest (num, den, percent, se, CI)
#' @param pct_txt character - name for the percent column
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
stats_wide_xl <- function(df_stats, file = NULL,
                          stats = c("num", "percent", "CI"),
                          pct_txt = "pct", append = FALSE, shtnm = NULL) {

  require(gt)
  require(openxlsx)


  params <- brfss.params()

  coi <-attr(df_stats,"coi")
  quest <-attr(df_stats,"question")
  weighted <-attr(df_stats,"weighted")

  if(is.null(shtnm)) shtnm <- coi

  df_stats_wide <- widen_stats(df_stats, stats = stats)

  stats <-attr(df_stats_wide, "stats")
  has.den <- "den" %in% stats
  has.num <- "num" %in% stats

  stats <- stats[!stats %in% "den"]

  stats[stats=="percent"] <- pct_txt

  nstats <- length(stats)

  spanners <- attr(df_stats_wide, "spanners")
  nspanners <- length(spanners)

  if(append) wb <- openxlsx::loadWorkbook(file = file)
  else  wb <- createWorkbook()

  ws <- addWorksheet(wb,shtnm)

  nrows <- nrow(df_stats_wide)
  ncols <- ncol(df_stats_wide)
  data_col <- 3 + as.integer(has.den)

  ##  styles

  sty_rowgr <- createStyle(fontSize = 12,halign = "LEFT", indent = 2 ,
                           border = "TopBottom", textDecoration = "bold")

  sty_hdr1 <- createStyle(fontSize = 14, halign = "CENTER", textDecoration = "bold")
  sty_hdr2 <- createStyle(fontSize = 13,halign = "CENTER", textDecoration = "bold")
  sty_hdr3 <- createStyle(fontSize = 13,halign = "CENTER", wrapText = TRUE,
                          textDecoration = "bold")

  sty_spnnr <- createStyle(fontSize = 13,halign = "CENTER", valign = "BOTTOM",
                           wrapText = TRUE, border = "TopBottomLeftRight",
                           textDecoration = "bold")

  sty_cnames <- createStyle(fontSize = 13,halign = "CENTER", valign = "BOTTOM",
                            wrapText = TRUE, border = "TopBottomLeftRight",
                            textDecoration = "bold")

  sty_text <- createStyle(fontSize = 12,halign = "CENTER",
                          border = "TopBottomLeftRight")
  sty_int <- createStyle(fontSize = 12,halign = "RIGHT", indent = 2 ,
                         border = "TopBottomLeftRight")



  ##    header - 3 lines
  writeData(wb,shtnm,paste0(geog_name(params["geog"]), " BRFSS"),1,1)
  writeData(wb,shtnm,params["year"],1,2)
  writeData(wb,shtnm,paste0("Variable: [",coi,"]"),1,3)
  writeData(wb = wb,sheet = shtnm, x = paste0(quest),startCol = 1,startRow = 4,
            borders = "rows")

  mergeCells(wb,shtnm, 1:ncols, 1)
  mergeCells(wb, shtnm, 1:ncols, 2)
  mergeCells(wb, shtnm, 1:ncols, 3)
  mergeCells(wb, shtnm, 1:ncols, 4)

  addStyle(wb, shtnm, sty_hdr1, 1, 1)
  addStyle(wb, shtnm, sty_hdr2, 2, 1)
  addStyle(wb, shtnm, sty_hdr2, 3, 1)
  addStyle(wb, shtnm, sty_hdr3, 4, 1)

  ##  spanners and column headings

  spnnr_row <- 6

  icol <- data_col

  setRowHeights(wb = wb, sheet =  shtnm, rows = spnnr_row, heights= 40)
  sapply(spanners, function(spanner) {
    icols <- icol:(icol+nstats-1)
    mergeCells(wb = wb, sheet =  shtnm,cols = icols ,rows = spnnr_row)
    writeData(wb = wb,sheet =  shtnm,x = spanner,startCol = icol,startRow = spnnr_row)
    addStyle(wb = wb, sheet =  shtnm, style = sty_spnnr,rows = spnnr_row, cols = icols)

    writeData(wb = wb,sheet =  shtnm,x = stats %>% as.data.frame %>% t(),
              startCol = icol,startRow = spnnr_row+1, colNames = FALSE,
              borders = "all")
    addStyle(wb = wb, sheet =  shtnm, style = sty_cnames,rows = spnnr_row+1,
             cols = icols)

    icol <<- icol + nstats
  })

  ##     write body of stats data

  data_row <- spnnr_row+2

  subvars <- df_stats_wide %>%
    pull(subvar) %>%
    unique()

  idata_row <- data_row

  sapply(subvars, function(subvar) {

    #  if it's not the "All respondents" group,
    #    write the row group name in it's own row, style it, and merge it across

    if(nchar(subvar)>0) {

      lbl <- column_label(subvar) %>% ifelse(length(.)>0,.,subvar)

      writeData(wb = wb,sheet =  shtnm,x = lbl,
                startCol = 1,startRow = idata_row, colNames = FALSE,
                borders = "all" )

      addStyle(wb = wb, sheet =  shtnm, style = sty_rowgr, rows = idata_row,
               cols = 1)


      mergeCells(wb, shtnm, 1:ncols, idata_row)

      inc_row <- 1
    } else  inc_row <- 0

    #  get the data for the row group and remove the name from the data so it only shows up
    #  in it's own row above

    df_sub <- df_stats_wide %>%
      filter(subvar == {{subvar}}) %>%
      mutate(subvar = "")

    #   write the data for the row group

    writeData(wb = wb,sheet =  shtnm,x = df_sub,
              startCol = 1,startRow = idata_row + inc_row, colNames = FALSE,
              borders = "all" )

    # advance the keeper of the next row

    idata_row <<- idata_row + nrow(df_sub) + inc_row
  })


  ##  format body of stats data


  icol <- data_col
  irows <- data_row:(data_row+nrows-1 + length(subvars) - as.integer("" %in% subvars))

  sapply(stats, function(stat) {

    if(stat == "CI") fmt <- sty_text else fmt <- sty_int

    sapply(0:(nspanners-1), function(index) {
      mycol <- icol + index * nstats
      addStyle(wb = wb, sheet =  shtnm, style = fmt,rows = irows,
               cols = mycol)

    })
    icol <<- icol+1
  })

  setColWidths(wb = wb, sheet =  shtnm, cols = 1, widths = 1)

  addStyle(wb = wb, sheet =  shtnm,
           style = createStyle(border = c("top","bottom"),
                               textDecoration = "bold"),
           rows = irows,
           cols = 1)

  addStyle(wb = wb, sheet =  shtnm,
           style = createStyle(border = c("top","bottom","right")),
           rows = irows,
           cols = 2)

  if(has.den) {
    writeData(wb = wb,sheet =  shtnm,x = "den",
              startCol = 3, startRow = spnnr_row+1, colNames = FALSE,
              borders = "all")

    addStyle(wb = wb, sheet =  shtnm, style = sty_cnames,rows = spnnr_row+1,
             cols = 3)

    addStyle(wb = wb, sheet =  shtnm, style = sty_int, rows = irows,
             cols = 3)

  }

  ##  adding weighting footnote

  if(weighted) {

    ##   fix the superscript
    txt <- paste0("The data are weighted.")

    if(has.den || has.num) txt <- paste0(txt, "The percents calculated ",
                                         "are not equal to numerator(num) divided by denominator(den)")

    ## append superscript to percent column name
    wb <- wb_post_superscript(wb,pct_txt,1)

    irow <- max(irows)+ 1

    writeData(wb = wb,sheet =  shtnm,
              x = txt,
              startCol = 1, startRow = irow,  colNames = FALSE)

    wb <- wb_pre_superscript(wb,txt,1)

    mergeCells(wb, shtnm, 1:ncols, irow)
    setRowHeights(wb = wb, sheet =  shtnm, rows = irow, heights= 20)

    addStyle(wb = wb, sheet =  shtnm,
             style = createStyle(fontSize = 7, wrapText = TRUE, valign = "top"),
             rows = irow,
             cols = 1)
  }

  if("ci" %in% stats) {
    ## handle superscript for CI description

    wb <- wb_post_superscript(wb,"CI",2)

    irow <- irow + 1

    txt <- paste0("95% Confidence Interval")

    writeData(wb = wb,sheet =  shtnm,
              x = txt,
              startCol = 1, startRow = irow,  colNames = FALSE)


    wb <- wb_pre_superscript(wb,txt,2)

  }

  max_char <- df_stats %>% pull(subset) %>% nchar() %>% max()

  setColWidths(wb,1,2,max_char)

  saveWorkbook(wb,file = file, overwrite = TRUE)


}

wb_post_superscript <- function(wb, text, num) {

  vert <- '<vertAlign val=\"superscript\"/>'

  str_index <- wb$sharedStrings %>% grep(paste0(">",text,"<"),., fixed = TRUE)

  txt <- wb$sharedStrings[str_index]

  txt <- paste0("<si>",
                "<r>",
                "<rPr></rPr>",
                "<t xml:space=\"preserve\">", text,"</t>",
                "</r>",
                "<r>",
                "<rPr>",vert,"</rPr>",
                "<t xml:space=\"preserve\">",num,"</t>",
                "</r>",
                "</si>")

  wb$sharedStrings[str_index] <- txt

  wb
}


wb_pre_superscript <- function(wb, text, num) {

  vert <- '<vertAlign val=\"superscript\"/>'

  str_index <- wb$sharedStrings %>% grep(paste0(">",text,"<"),., fixed = TRUE)

  txt <- wb$sharedStrings[str_index]

  txt <- paste0("<si>",
                "<r>",
                "<rPr>",vert,"</rPr>",
                "<t xml:space=\"preserve\">",num,"</t>",
                "</r>",
                "<r>",
                "<rPr></rPr>",
                "<t xml:space=\"preserve\">",text,"</t>",
                "</r>",
                "</si>")
  wb$sharedStrings[str_index] <- txt

  wb
}


######################################################################################
##
##    excel version of stats table

#' Stats Table - Excel
#'
#' Produces a table of the stats
#'
#' @param df_stats
#' @param file
#' @param stats
#' @param pct_txt
#' @param append
#' @param shtnm
#'
#' @return
#' @export
#'
#' @examples
stats_xl <- function(df_stats, file = NULL,
                     stats = c("num", "percent", "CI"),
                     pct_txt = "pct", append = FALSE, shtnm = NULL) {

  require(gt)
  require(openxlsx)

  params <- brfss.params()

  coi <-attr(df_stats,"coi")
  quest <-attr(df_stats,"question")
  weighted <-attr(df_stats,"weighted")

  if(is.null(shtnm)) shtnm <- coi

  #stats <-attr(df_stats, "stats")
  has.den <- "den" %in% stats

  stats <- stats[!stats %in% "den"]

  stats[stats=="percent"] <- pct_txt

  nstats <- length(stats)

  if(append) wb <- openxlsx::loadWorkbook(file = file)
  else  wb <- createWorkbook()

  ws <- addWorksheet(wb,shtnm)

  nrows <- nrow(df_stats)
  ncols <- ncol(df_stats)
  data_col <- 3 + as.integer(has.den)

  ##  styles

  sty_rowgr <- createStyle(fontSize = 12,halign = "LEFT", indent = 2 ,
                           border = "TopBottom", textDecoration = "bold")

  sty_hdr1 <- createStyle(fontSize = 14, halign = "CENTER", textDecoration = "bold")
  sty_hdr2 <- createStyle(fontSize = 13,halign = "CENTER", textDecoration = "bold")
  sty_hdr3 <- createStyle(fontSize = 13,halign = "CENTER", wrapText = TRUE,
                          textDecoration = "bold")

  sty_spnnr <- createStyle(fontSize = 13,halign = "CENTER", valign = "BOTTOM",
                           wrapText = TRUE, border = "TopBottomLeftRight",
                           textDecoration = "bold")

  sty_cnames <- createStyle(fontSize = 13,halign = "CENTER", valign = "BOTTOM",
                            wrapText = TRUE, border = "TopBottomLeftRight",
                            textDecoration = "bold")

  sty_text <- createStyle(fontSize = 12,halign = "CENTER",
                          border = "TopBottomLeftRight")
  sty_int <- createStyle(fontSize = 12,halign = "RIGHT", indent = 2 ,
                         border = "TopBottomLeftRight")



  ##    header - 3 lines
  writeData(wb,shtnm,paste0(geog_name(params["geog"]), " BRFSS"),1,1)
  writeData(wb,shtnm,params["year"],1,2)
  writeData(wb,shtnm,paste0("Variable: [",coi,"]"),1,3)
  writeData(wb = wb,sheet = shtnm, x = paste0(quest),startCol = 1,startRow = 4,
            borders = "rows")

  mergeCells(wb,shtnm, 1:ncols, 1)
  mergeCells(wb, shtnm, 1:ncols, 2)
  mergeCells(wb, shtnm, 1:ncols, 3)
  mergeCells(wb, shtnm, 1:ncols, 4)

  addStyle(wb, shtnm, sty_hdr1, 1, 1)
  addStyle(wb, shtnm, sty_hdr2, 2, 1)
  addStyle(wb, shtnm, sty_hdr2, 3, 1)
  addStyle(wb, shtnm, sty_hdr3, 4, 1)

  ##  spanners and column headings

  spnnr_row <- 6

  data_row <- spnnr_row+2

  subvars <- df_stats %>%
    pull(subvar) %>%
    unique()

  idata_row <- data_row

  if(!"se" %in% stats) df_stats <- df_stats %>% select(-se)


  df_stats <- df_stats %>%
    rename({{pct_txt}} := percent) %>%
    mutate(CI = paste0(CI_lower, " - ", CI_upper)) %>%
    select(-starts_with("CI_")) %>%
    select(matches(c("subvar","subset","response",stats, pct_txt)))

  # calculate width of first column

  max_char <- df_stats %>% pull(response) %>% nchar() %>% max()
  width1 <- max_char * 10


  writeData(wb = wb, sheet =  shtnm, x = df_stats,
            startCol = 1,startRow = idata_row,
            borders = "all" ,headerStyle = sty_hdr2)

  openxlsx::setColWidths(wb = wb, sheet =  shtnm, cols = 2:3, widths = "auto")
  #

  saveWorkbook(wb,file = file, overwrite = TRUE)


}

xl_add_coi <- function(df_data, coi, subsets, stats, file, append = FALSE) {

  require(brfss)


  df_stats <-survey_stats(df_data = df_data, coi = coi,
                          subsets = subsets,
                          subset_by = NULL, pct = TRUE, digits = 1)

  stats_wide_htm(df_stats, stats = stats)

  df_stats %>%
    stats_wide_xl(stats = stats, file = file, pct_txt = "pct", shtnm = paste0(coi,"_wide"), append = append)

  df_stats %>%
    stats_xl(stats = stats, file = file, pct_txt = "pct", shtnm = coi, append = TRUE)

}
