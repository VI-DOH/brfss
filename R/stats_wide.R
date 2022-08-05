
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
stats_wide <- function(df_stats) {
  require(gt)

  params <- brfss.params()

  coi <-attr(df_stats$response,"coi")

  df_stats <- df_stats %>%
    mutate(CI = paste0(CI_lower, " - ", CI_upper))

  df_stats_wide <- df_stats %>%

    # stretch measure horizontally with variable mean
    reshape2::dcast(subvar + subset ~ response, value.var = c("num")) %>%

    # add CI_lower to right end
    left_join(df_stats %>%
                reshape2::dcast(subvar + subset ~ response, value.var = c("percent")),
              by = c("subset", "subvar"))  %>%

    rename_with(~ gsub(".x", ".num",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", "",.x, fixed = TRUE))%>%

    left_join(df_stats %>%
                reshape2::dcast(subvar + subset ~ response, value.var = c("CI")),
              by = c("subset", "subvar"))  %>%

    rename_with(~ gsub(".x", ".percent",.x, fixed = TRUE)) %>%
    rename_with(~ gsub(".y", ".CI",.x, fixed = TRUE))

  ##################################################
  ##
  ##  get the correct order of the factors

  coi_data <- coi_data(coi = coi ) %>% pull({{coi}}) %>% droplevels()

  fctrs <- levels(coi_data)
  fctrs <- fctrs[fctrs%in%unique(df_stats$response)]

  stats <- c("num", "percent", "CI") #,"CI_lower","CI_upper")

  df_col_order <- data.frame(name = colnames(df_stats_wide)) %>%
    mutate(is_meas = grepl(".",name, fixed=TRUE)) %>%
    mutate(var = gsub("(.*)\\.(.*)","\\1",name))%>%
    mutate(stat = gsub("(.*)\\.(.*)","\\2",name)) %>%
    mutate(varpos = ifelse(is_meas,match(var,fctrs),0)) %>%
    mutate(statpos = ifelse(is_meas,match(stat,stats),0)) %>%
    arrange(varpos,statpos)

  col_order <- df_col_order %>%
    pull(name)

  n_non <- df_col_order %>% filter(varpos == 0) %>% nrow

  df_spanners <- df_col_order %>%
    filter(is_meas) %>%
    group_by(var,varpos) %>%
    summarize(tot = max(statpos)) %>%
    arrange(varpos)


  df_stats_wide <- df_stats_wide  %>% select(all_of(col_order))


  lbls <- as.list(mapply(function(nm,val) {
    x <- val
    x
  }, df_col_order$name, df_col_order$stat))

  lbls[lbls=="percent"] <- "%"

  ############################################
  ##
  ##    build the table

  gt_stats <- gt::gt(df_stats_wide,
                     groupname_col= "subvar",
                     rowname_col="subset") %>%
    gt::tab_header(title = htmltools::HTML(paste0(geog_name(params["geog"]), " BRFSS<br>",
                                       params["year"])),
                   subtitle = paste0("Variable: ", coi)) %>%

    tab_stubhead(label = "Group") %>%

    cols_label( .list =lbls) %>%

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
          # ),
          # cell_borders(
          #   sides = c("right"),
          #   color = "black",
          #   weight = px(2),
          #   style = "solid"
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
          # ),
          # cell_borders(
          #   sides = c("right"),
          #   color = "black",
          #   weight = px(2),
          #   style = "solid"
        )),
      locations = list(

        cells_column_spanners(

        ),
        cells_column_labels(

        )
      )
    ) %>%

    tab_style(
      style = cell_text(align = "left", indent = px(20)),
      locations = cells_stub()
    )


  sapply(1:max(df_col_order$varpos), function(i) {

    gt_stats <<- gt_stats %>%

      tab_spanner(label = df_spanners[i,"var"],
                  columns = df_col_order %>% filter(varpos == i) %>% pull(name) )

  })

  gt_stats
}


