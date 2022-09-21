
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

  ##################################################
  ##
  ##  get the correct order of the factors

  res <- try({
    coi_data <- coi_data(coi = coi ) %>% pull({{coi}}) %>% droplevels()

    fctrs <- levels(coi_data)
    fctrs <- fctrs[fctrs%in%unique(df_stats$response)]
  })

  if(inherits(res, "try-error")) {
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
