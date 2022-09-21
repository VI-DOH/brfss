

######################################################################################
##
##    html version of wide table


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
#'
stats_wide_htm <- function(df_stats, stats = c("num", "percent", "CI"),
                           pct_txt = "pct") {

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


  gt_stats

}
