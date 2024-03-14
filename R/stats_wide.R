
#' Stretch Stats Table
#'
#' Stretch (cast) the stats table horizontally such that measure values become the
#' column names. They are actually spanner values with the stat name (pct, etc.) as the
#' column label
#'
#' @param df_stats - data.frame
#'
#' @return
#' @export
#'
#' @examples

############################################################################
##
##      widen_stats
##

widen_stats <- function(df_stats, stats = c("num", "percent", "CI")){

  coi <-attr(df_stats,"coi")

  df_stats <- df_stats %>%
    mutate(CI = paste0(CI_lower, " - ", CI_upper)) %>%
    select(-starts_with("CI_"))

  stat_cols <- colnames(df_stats)[(df_stats %>% colnames() %>% grep("den",.)):ncol(df_stats)]
  rem_cols <- stat_cols[!stat_cols%in%stats]


  df_stats_wide <- df_stats %>%

    # stretch measure horizontally with variable mean
    reshape2::dcast(subvar + subset + den ~ response, value.var = c("num")) %>%

    # add CI_lower to right end
    left_join(df_stats %>%
                reshape2::dcast(subvar + subset + den ~ response, value.var = c("percent")),
              by = c("subset", "subvar", "den"))  %>%

    rename_with(~ gsub(".x", ".num",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", "",.x, fixed = TRUE))%>%

    left_join(df_stats %>%
                reshape2::dcast(subvar + subset + den ~ response, value.var = c("CI")),
              by = c("subset", "subvar", "den"))  %>%

    rename_with(~ gsub(".x", ".percent",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", ".CI",.x, fixed = TRUE))


  #########################################################################
  ##
  ##    reorder rows back to original
  ##      dcast is alphabetically ordering the subvars

  df_ord <- df_stats %>% select(subvar, subset) %>% distinct()

  df_stats_wide <- df_ord %>% left_join(df_stats_wide, by = c("subvar", "subset"))


  ##################################################
  ##
  ##  get the correct order of the factors

  res <- tryCatch({
    coi_data <- coi_data(coi = coi ) %>% pull({{coi}}) %>% droplevels()

    fctrs <- levels(coi_data)
    fctrs <- fctrs[fctrs%in%unique(df_stats$response)]
  },
  error=function(cond) {
    # message(paste("URL does not seem to exist:", url))
    # message("Here's the original error message:")
    # message(cond)
    # Choose a return value in case of error

    return(cond)
  })

  if(inherits(res, "error")) {
    fctrs <- unique(df_stats$response)
  }
  #  stats <- c("num", "percent", "CI") #,"CI_lower","CI_upper")

  df_col_order <- data.frame(name = colnames(df_stats_wide)) %>%
    mutate(is_meas = grepl(".",name, fixed=TRUE)) %>%
    mutate(var = gsub("(.*)\\.(.*)","\\1",name))%>%
    mutate(stat = gsub("(.*)\\.(.*)","\\2",name)) %>%
    mutate(varpos = ifelse(is_meas,match(var,fctrs),0)) %>%
    mutate(statpos = ifelse(is_meas,match(stat,stats),0)) %>%
    arrange(varpos,statpos)

  col_order <- df_col_order %>%
    pull(name)

  # n_non <- df_col_order %>% filter(varpos == 0) %>% nrow


  df_spanners <- df_col_order %>%
    filter(is_meas) %>%
    group_by(var,varpos) %>%
    summarize(tot = max(statpos), .groups = "keep") %>%
    arrange(varpos)

  df_stats_wide <- df_stats_wide  %>%
    select(all_of(col_order))%>%
    select(-ends_with(rem_cols))

  lbls <- sapply(colnames(df_stats_wide), function(cname){
    is.stat <- grepl(".",cname,fixed = TRUE)

    if(is.stat) {

      stat <- gsub("(.*)[.](.*)","\\2",cname)
      attr(df_stats_wide[[cname]],"stat") <<- stat

      attr(df_stats_wide[[cname]],"spanner") <<-
        gsub("(.*)[.](.*)","\\1",cname)

      return(stat)

    } else {

      return(cname)
    }

  })

  #df_stats_wide <- df_stats_wide %>% select(-ends_with(rem_cols))

  attr(df_stats_wide,"spanners") <- df_spanners$var
  attr(df_stats_wide,"stats") <- stats
  attr(df_stats_wide,"coi") <- coi
  attr(df_stats_wide,"lbls") <- lbls

  df_stats_wide
}



######################################################################################
##
##   wide stats table


#' Stretch Stats Table
#'
#' Stretch (cast) the stats table horizontally such that measure values become the
#' column names. They are actually spanner values with the stat name (pct, etc.) as the
#' column label
#'
#' @param df_stats
#' @param stats
#' @param pct_txt
#' @param latex
#'
#' @return
#' @export
#'
#' @examples
stats_wide <- function(df_stats, stats = c("num", "percent", "CI"),
                       pct_txt = "pct", latex = FALSE, title = NULL, subtitle = NULL) {

  require(gt)

  params <- brfss.params()

  coi <-attr(df_stats,"coi")

  df_stats_wide <- widen_stats(df_stats, stats = stats)

  #  stats <- c("num", "percent", "CI") #,"CI_lower","CI_upper")

  spanners <- attr(df_stats_wide,"spanners")

  lbls <- attr(df_stats_wide,"lbls")

  lbls[lbls=="percent"] <- pct_txt

  # spanner calcs

  cols <- gsub("(.*)[.].*","\\1",colnames(df_stats_wide))

  spanner_cols <- as.data.frame(mapply(function(spanner)  {
    which(cols == spanner)
  }, spanners))

  # spanners <- sapply(spanners,function(spanner)  {
  #   nl_at(spanner,30)
  # }) %>% unname()

  # calculate width of first column

  max_char <- df_stats %>% pull(subset) %>% nchar() %>% max()
  width1 <- max_char * 10

  #options(gt.spanner_id.sep = "\n")

  ############################################
  ##
  ##    build the table

  gt_stats <- gt::gt(df_stats_wide, id = "stats_wide",
                     groupname_col= "subvar",
                     rowname_col="subset") %>%
    gt::tab_header(title = htmltools::HTML(paste0(geog_name(params["geog"]),
                                                  " BRFSS<br>",
                                                  params["year"])),
                   subtitle = paste0("Variable: ", coi)) %>%

    tab_stubhead(label = "Group") %>%

    cols_label( .list =lbls) %>%

    cols_width(
      as.formula(paste0("matches('subset') ~ px(",width1,")")),
      matches("num") ~ px(50),
      matches("CI") ~ px(110),
      everything() ~ px(60)
    ) %>%

    cols_align(
      align = c("center"),
      columns = matches("CI")
    ) %>%

    tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = "black",
          weight = px(1),
          style = "solid"

        )),
      locations = list(
        cells_body(
          columns = contains("num")
        )
      )
    ) %>%

    tab_style(
      style = list(
        cell_borders(
          color = "black",
          weight = px(1),
          style = "solid"

        ),
        cell_text(v_align = "bottom", align = "center")
      ),
      locations = list(

        cells_column_labels(

        )
      )
    )  %>%

    tab_style(
      style = cell_text(align = "left", indent = px(20)),
      locations = cells_stub()
    ) %>%
    tab_options(data_row.padding.horizontal = px(15))



  mapply( function(spanner, cols) {

    gt_stats <<- gt_stats %>%

      tab_spanner(label = spanner,
                  columns = all_of(cols))

  },spanners, spanner_cols)

  gt_stats <- gt_stats %>%

    tab_style(
      style = list(
        cell_borders(
          color = "black",
          weight = px(1),
          style = "solid"

        ),
        cell_text(v_align = "bottom")
      ),
      locations = list(

        cells_column_spanners(

        )
      )
    )


  if(latex) {
    #gt_stats$`_heading`$title <- gsub("<br>","\n  ",gt_stats$`_heading`$title)
    gt_stats <- as_latex(gt_stats)
  }

  gt_stats

}

#' Title
#'
#' @param df_stats
#' @param stats
#' @param pct_txt
#'
#' @return
#' @export
#'
#' @examples
stats_wide_ltx <- function(df_stats, stats ,
                           pct_txt) {

  stats_wide(df_stats = df_stats, stats = stats,
             pct_txt = pct_txt, latex = TRUE)

}

#' Title
#'
#' @param df_stats
#' @param stats
#' @param pct_txt
#'
#' @return
#' @export
#'
#' @examples
stats_wide_htm <- function(df_stats, stats=c("pct"),
                           pct_txt="Pct") {

  stats_wide(df_stats = df_stats, stats = stats,
             pct_txt = pct_txt, latex = FALSE)

}


