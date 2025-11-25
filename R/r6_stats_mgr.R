
library(R6)
#' Layout_Mgr R6 Class
#'
#' @export
StatsMgr <-
  R6Class(classname = "StatsMgr",


          ################################################################################
          ##
          ##           PRIVATE
          ##
          ################################################################################

          private = list(
            data_pvt = NULL,
            coi_pvt = NULL,
            exclude_pvt = c("Don.*t|Refuse"),
            subsets_pvt = NULL,
            subpopulation_pvt = NULL,
            sub_exclude_pvt = c("Don.*t|Refuse"),
            conf_pvt =.95,
            weighted_pvt = TRUE,
            pct_pvt = FALSE,
            digits_pvt = 99,
            stats_pvt = c("subvar","subset","response","num","den",
                          "percent","se","CI_lower","CI_upper","cv",
                          "percent_unwtd","num_wtd","den_wtd"),
            my_stats_pvt = NULL #,

            ###########################################################
            ##
            ##    get stats for a coi with subsetting

            # stats_w_subs = function(des) {
            #
            #   pct = private$pct_pvt
            #   digits = private$digits_pvt
            #   conf = private$conf_pvt
            #
            #   mult <- ifelse(pct,100,1)
            #
            #   coi <- names(des$variables[1])
            #   subset <- names(des$variables[2])
            #   subvar <- names(des$variables[2])
            #
            #   frmla1<- reformulate(c(coi) %>% paste0("`",.,"`"))
            #   frmla2<- reformulate(c(subset) %>% paste0("`",.,"`"))
            #
            #   mysvymean<-survey::svyby(frmla1,frmla2,des,svymean)
            #
            #   mysvytotal <-survey::svyby(frmla1,frmla2,des,svytotal) %>%
            #     select(-starts_with("se."))  %>%
            #     rename_with(~gsub(coi,"",.x))  %>%
            #     rename_with(~gsub("`","",.x))  %>%
            #     tidyr::pivot_longer(
            #       cols = -all_of(subvar),
            #       names_to = "response",
            #       values_to = "num_wtd"
            #     )  %>%
            #     rename(subset = 1) %>%
            #     mutate(subset = as.character(subset))
            #
            #   df_wtd <- mysvytotal  %>%
            #     group_by(subset) %>%
            #     summarize(den_wtd = sum(num_wtd)) %>%
            #     as.data.frame() %>%
            #     mutate(subset = as.character(subset))
            #
            #   mysvytotal <- mysvytotal %>%
            #     left_join(df_wtd, by = join_by(subset))
            #
            #   mysvycounts<-survey::svyby(frmla1,frmla2,des,unwtd.count)
            #
            #   df_dens <- mysvycounts %>% select(-se) %>%
            #     rename( den = counts) %>%
            #     rename(subset = {{subset}}) %>%
            #     mutate(subset = as.character(subset))
            #
            #   df_nums <- data.frame(des$variables[1],des$variables[2],check.names = FALSE) %>%
            #     group_by_at(c(coi, subset)) %>%
            #     summarise(num=n()) %>%
            #     rename(response = {{coi}}, subset = {{subset}}) %>%
            #     mutate(subset = as.character(subset))
            #
            #   myci <- private$svyci(mysvymean, conf = private$conf_pvt) %>%
            #     private$remove_coi(coi) %>%
            #     mutate(subset = gsub("(.*)[:](.*)","\\1", response)) %>%
            #     mutate(response = gsub("(.*)[:](.*)","\\2", response)) %>%
            #     mutate(across(where(is.numeric), ~ . * mult)) %>%
            #     mutate(across(where(is.numeric), round, digits))
            #
            #   df_stats <- as.data.frame(mysvymean) %>%
            #     rename_with(~gsub(coi,"",.x)) %>%
            #     rename_with(~gsub("`","",.x))  %>%
            #     mutate(across(where(is.numeric), ~ . * mult)) %>%
            #     mutate(across(where(is.numeric), round, digits))%>%
            #     tidyr::pivot_longer(
            #       cols = -all_of(subvar),
            #       names_to = "response",
            #       values_to = "value"
            #     ) %>%
            #     mutate(
            #       type = if_else(stringr::str_starts(response, "se\\."), "se", "percent"),
            #       response = stringr::str_remove(response, "^se\\.")
            #     ) %>%
            #     tidyr::pivot_wider(
            #       names_from = type,
            #       values_from = value
            #     ) %>%
            #     rename(subset = 1) %>%
            #     mutate(subvar = {{subvar}}) %>%
            #     relocate(subvar) %>%
            #     mutate(subset = as.character(subset)) %>%
            #     left_join(myci, by = join_by(response, subset)) %>%
            #     mutate(cv = se/percent) %>%
            #     left_join(df_dens, by = join_by(subset))%>%
            #     left_join(df_nums, by = join_by(subset, response))   %>%
            #     mutate(percent_unwtd = round(num/den * mult, digits)) %>%
            #     left_join(mysvytotal, by = join_by(subset, response)) %>%
            #     as.data.frame() %>%
            #     relocate(c(num,den), .after = response)
            #
            #
            #   df_stats
            # },
            #
            # ###########################################################
            # ##
            # ##    get stats for just a coi
            #
            #
            # stats_no_subs =  function(des) {
            #
            #   frmla<- reformulate(names(des$variables) %>% paste0("`",.,"`"))
            #   mult <- ifelse(private$pct_pvt,100,1)
            #   #
            #   coi <- as.character(frmla)[2]
            #
            #   mysvymean<-survey::svymean(frmla,des,na.rm = T,deff = F)
            #
            #   mycv <- cv(mysvymean)%>% t()
            #   mycv <- data.frame(response = dimnames(mycv)[[2]] %>% gsub(coi,"",.),
            #                      cv = mycv %>% as.numeric())
            #
            #   myci <- private$svyci(mysvymean = mysvymean, conf = private$conf_pvt) %>%
            #     private$remove_coi(coi)%>%
            #     mutate(across(where(is.numeric), ~ . * mult)) %>%
            #     mutate(across(where(is.numeric), round, private$digits_pvt))
            #
            #   mysvymean <- mysvymean %>%
            #     as.data.frame() %>%
            #     mutate(response = rownames(.))  %>%
            #     private$remove_coi(coi) %>%
            #     rename(se = SE) %>%
            #     rename(percent = mean) %>%
            #     relocate(response) %>%
            #     tibble::remove_rownames() %>%
            #     mutate(across(where(is.numeric), ~ . * mult)) %>%
            #     mutate(across(where(is.numeric), round, private$digits_pvt))
            #
            #   mysvytotal<-survey::svytotal(frmla,des,na.rm = T,deff = F) %>%
            #     as.data.frame() %>%
            #     mutate(response = rownames(.))  %>%
            #     private$remove_coi(coi) %>%
            #     select(-SE) %>%
            #     rename(num_wtd = total)  %>%
            #     mutate(den_wtd = sum(num_wtd)) %>%
            #     relocate(response) %>%
            #     tibble::remove_rownames()
            #
            #   mysvycounts <- survey::svyby(formula = frmla, by = frmla, design = des,
            #                                FUN = unwtd.count)%>%
            #     as.data.frame() %>%
            #     select(-se) %>%
            #     rename(response = 1)  %>%
            #     rename(num = counts) %>%
            #     mutate(den = sum(num))  %>%
            #     mutate(percent_unwtd = round(num/den * mult, private$digits_pvt)) %>%
            #     relocate(response) %>%
            #     tibble::remove_rownames()
            #
            #   names(mysvymean) <- gsub(coi,"",names(mysvymean))
            #
            #   df_stats <- mysvycounts  %>%
            #     left_join(mysvytotal, by = join_by(response)) %>%
            #     left_join(mysvymean, by = join_by(response)) %>%
            #     left_join(mycv, by = join_by(response)) %>%
            #     left_join(myci, by = join_by(response))%>%
            #     mutate(subvar = "") %>%
            #     mutate(subset = "All Respondents")  %>%
            #     relocate(c(subvar,subset))
            #
            #   rownames(df_stats)<- NULL
            #
            #   df_stats
            # },
            #
            # # ---------------------------------------------------------
            #
            # coi_data = function() {
            #
            #   df_data= private$data_pvt
            #   coi = private$coi_pvt
            #   subsets = private$subsets_pvt
            #   exclude = private$exclude_pvt
            #
            #   #df_brfss <- brfss_data(year,geog)
            #   df_brfss <- data.frame() #   coi_data(coi, year,geog,version = 0)
            #
            #   df_brfss <- purrr::map(0:highest_version(),function(ver) {
            #     private$coi_data_vers(version = ver)
            #   })
            #
            #
            #   df_brfss <- df_brfss %>%
            #     bind_rows()
            #
            #   voi <- df_brfss %>% pull(vers) %>% unique()
            #
            #   df_resp <- responses_by_geog()
            #
            #   if(!is.null(df_resp)) {
            #     df_resp <- df_resp %>%
            #       dplyr::filter(version %in% voi) %>%
            #       dplyr::mutate(pct = responses/sum(responses))
            #   } else {
            #     df_resp <- data.frame(version = 0, pct = 1.0)
            #   }
            #
            #   df_brfss <- df_brfss %>%
            #     dplyr::left_join(df_resp, by = c("vers" = "version")) %>%
            #     dplyr::mutate(FINAL_WT = FINAL_WT * pct) %>%
            #     dplyr::select(matches(coi), FINAL_WT, STRATUM, all_of(subsets))%>%
            #     dplyr::rename(coi = {{coi}})%>%
            #     dplyr::mutate(coi = replace(coi, grep(exclude,coi),NA)) %>%
            #     filter(!is.na(coi))
            #
            #   if (is.factor(df_brfss %>% pull(coi)))
            #     df_brfss <- df_brfss %>% dplyr::mutate(coi = droplevels(coi))
            #
            #   df_brfss %>%
            #     dplyr::rename({{coi}} := coi)
            # },
            #
            # # ---------------------------------------------------------
            #
            # coi_data_vers = function(version) {
            #
            #   df_data= private$data_pvt
            #   coi = private$coi_pvt
            #   subsets = private$subsets_pvt
            #
            #
            #   vwt <- apply.pattern("weight_col",VERS=version)
            #   stratum <- apply.pattern("stratum_col")
            #
            #   brfss.params(version = version)
            #   if(is.null(df_data)) df_data <- brfss_geog_data()
            #
            #
            #   if(is.null(df_data)) return(data.frame())
            #
            #   df_data %>%
            #     dplyr::rename(FINAL_WT = {{vwt}}) %>%
            #     dplyr::rename(STRATUM = {{stratum}}) %>%
            #     dplyr::select(all_of(coi), FINAL_WT, STRATUM, all_of(subsets)) %>%
            #     dplyr::mutate(vers = {{version}})
            #
            #
            #
            # },
            #
            # remove_coi = function(df, coi) {
            #
            #   df %>%
            #     mutate(response = gsub(coi,"",response)) %>%
            #     mutate(response = gsub("\`","",response))
            # },
            #
            # svyci = function(mysvymean, conf) {
            #
            #   as.data.frame(confint(mysvymean,level = conf))%>%
            #     rename(CI_lower = 1, CI_upper = 2) %>%
            #     mutate(response = rownames(.)) %>%
            #     tibble::remove_rownames()
            #
            # }

          ),

          ################################################################################
          ##
          ##           PUBLIC
          ##
          ################################################################################

          public = list(
            initialize = function(data = NULL, coi = "", exclude = c("Don.*t|Refuse"),
                                  subsets = NULL,
                                  sub_exclude = c("Don.*t|Refuse"),
                                  subpopulation = NULL,
                                  conf =.95, weighted = TRUE, pct = FALSE, digits = 99 ) {

              private$data_pvt <- data
              private$coi_pvt <- coi
              private$exclude_pvt <- exclude
              private$subsets_pvt <-subsets
              private$subpopulation_pvt <- subpopulation
              private$sub_exclude_pvt <-sub_exclude
              private$conf_pvt <-conf
              private$weighted_pvt <- weighted
              private$pct_pvt <- pct
              private$digits_pvt <- digits

              private$my_stats_pvt <- private$stats_pvt

            },

            remove_stats = function(pttrn) {

              private$my_stats_pvt <- private$my_stats_pvt %>% grep(pttrn, ., value = T, invert = T)
            },

            reset_stats = function() {

              private$my_stats_pvt <- private$stats_pvt
            },

            survey_stats = function(coi = NULL) {

              df <- brfss::survey_stats(
                df_data = private$data_pvt,
                coi = private$coi_pvt,
                exclude = private$exclude_pvt,
                subsets = private$subsets_pvt,
                subset_by = private$subset_by_pvt,
                sub_exclude = private$sub_exclude_pvt,
                conf = private$conf_pvt,
                weighted = private$weighted_pvt,
                pct = private$pct_pvt,
                digits = private$digits_pvt)

              rm <- setdiff(private$stats_pvt, private$my_stats_pvt)

              df <- df %>% filter(response != "dummy") %>%
                            select(-all_of(rm))
              return(df)

  #             if(!is.null(coi)) private$coi_pvt <- coi else coi <- private$coi_pvt
  #
  #             if(any(grepl("brfss_prepped|brfss_data",class(private$data_pvt)))) {
  #
  #               year <- private$data_pvt %>% attr("year")
  #               geog <- private$data_pvt %>% attr("geog")
  #
  #             } else {
  #               year <- NA
  #               geog = NA
  #             }
  #
  #             ## make sure that this column exists
  #
  #             if(!has_column(private$data_pvt, coi)) return(NULL)
  #
  #             ##    this is a kludgy way to remove the exclude ...
  #             ##      NULL would be better and that will be fixed
  #
  #             if(is.null(private$exclude_pvt)) private$exclude_pvt <- "^$"
  #
  #             ###########################################################
  #
  #
  #             coi_attrs <- private$data_pvt %>% pull({{coi}}) %>% attributes()
  #
  #             df_brfss <- private$coi_data()
  #
  #             test <- df_brfss %>% pull({{coi}})
  #
  #             if(!(is.factor(test) && length(levels(test))>1) ) {
  #               #return(NULL)
  #               df_brfss<- df_brfss %>% mutate({{coi}} := as.factor(.[[coi]]))
  #
  #             }
  #             # get data from
  #
  #             if(nrow(df_brfss)==0) {
  #               ret<-data.frame()
  #               return (ret)
  #             }
  #
  #             if(is.null(private$subsets_pvt)) {
  #               nsubs<-0
  #             } else {
  #               nsubs<-length(private$subsets_pvt)
  #             }
  #
  #             # only the valid values - remove the excludes
  #
  #             ##########################
  #             ##
  #             ##  survey package
  #             ##
  #             ##
  #             if(private$weighted_pvt) weighting<-reformulate("FINAL_WT")   else weighting=NULL
  #
  #             strata<-reformulate("STRATUM")   #else weights=NULL
  #
  #             #  ids<- reformulate(all_vals)
  #
  #             options(survey.lonely.psu = "adjust")
  #
  #             frmla<- reformulate(c(coi) %>% paste0("`",.,"`"))
  #
  #
  #             ## this will fail if there is only one level so
  #             ##    we must add a"dummy" response level
  #             ##      it will be removed before returning the data
  #
  #             levels <- levels(df_brfss %>% pull({{coi}}))
  #
  #             if(length(levels) == 1) {
  #               levels(df_brfss[,coi]) <- c(levels , 'dummy')
  #             }
  #
  #             des <- NULL
  #
  #             des <- tryCatch({
  #               survey::svydesign(ids = ~1,
  #                                 strata = strata,
  #                                 variables =  frmla,
  #                                 data = df_brfss,
  #                                 weights = weighting,
  #                                 deff=F)
  #             }, error = function(e) {
  #
  #               cat(" I caught an error \n  ----- ", e$message, "\n ------\n")
  #               return(NULL)
  #
  #             }, warning = function(w) {
  #
  #               cat(" warning", w$message, "\n")
  #               return(NULL)
  #
  #               # }, finally = function() {
  #               #   cat(" finally \n")
  #               #
  #             }  )
  #
  #             if(is.null(des)) {
  #               return(data.frame())
  #
  #             }
  #
  #             df_stats_main <- private$stats_no_subs(des)
  #
  #             df_subs <- data.frame()
  #
  #             if (nsubs > 0 ) {
  #
  #               df_subs <- purrr::map(private$subsets_pvt, function(subset){
  #
  #                 cols <- c(coi,subset)  %>% paste0("`",.,"`")
  #                 frmla<- reformulate(cols)
  #
  #                 des<-survey::svydesign(ids = ~1,
  #                                        strata = strata,
  #                                        variables =  frmla,
  #                                        data = df_brfss,
  #                                        weights = weighting,
  #                                        deff=F)
  #
  #
  #                 private$stats_w_subs(des)
  #
  #               }) %>% bind_rows()
  #
  #             }
  #
  #             df <- bind_rows(df_stats_main, df_subs )
  #
  # #            population <- pop_sex(private$data, private$coi) %>% gsub("ale","ales",.)
  #
  #
  #             # remove "dummy" response (in case there was only one level)
  #
  #             df <- df %>% filter(response != "dummy") %>%
  #               select(any_of(private$my_stats_pvt))
  #
  #             #  get the actual stat columns
  #
  #             statcols <- colnames(df)
  #             resp <- which(statcols == "response")
  #             statcols <-statcols %>%  tail(-resp)
  #
  #             ## return data.frame
  #
  #             structure(df,
  #                       class = c("brfss_stats", "data.frame"),
  #                       year = year,
  #                       geog = geog,
  #                       coi = private$coi_pvt,
  #                       stat_cols = statcols,
  #                       population = coi_attrs[["population"]],
  #                       section_type = coi_attrs[["section_type"]],
  #                       section_num = coi_attrs[["section_num"]],
  #                       section_index = coi_attrs[["section_index"]],
  #                       section_name = coi_attrs[["section_name"]],
  #                       question = coi_attrs[["question"]],
  #                       label = coi_attrs[["label"]],
  #                       weighted = private$weighted_pvt,
  #                       conf = private$conf_pvt)
             }




          ),

          ################################################################################
          ##
          ##           ACTIVE
          ##
          ################################################################################

          active = list(

            survey_data = function(value) {

              if(missing(value)) return(private$data_pvt)

              private$data_pvt <- value

            },

            coi = function(value) {

              if(missing(value)) return(private$coi_pvt)

              private$coi_pvt <- value

            },

            exclude = function(value) {

              if(missing(value)) return(private$exclude_pvt)

              private$exclude_pvt <- value

            },

            subsets = function(value) {

              if(missing(value)) return(private$subsets_pvt)

              private$subsets_pvt <- value

            },

            subpopulation = function(value) {

              if(missing(value)) return(private$subpopulation_pvt)

              private$subpopulation_pvt <- value

            },

            sub_exclude =function(value) {

              if(missing(value)) return(private$sub_exclude_pvt)

              private$sub_exclude_pvt <- value

            },

            conf =function(value) {

              if(missing(value)) return(private$conf_pvt)

              private$conf_pvt <- value

            },

            weighted = function(value) {

              if(missing(value)) return(private$weighted_pvt)

              private$weighted_pvt <- value

            },

            pct = function(value) {

              if(missing(value)) return(private$pct_pvt)

              private$pct_pvt <- value

            },

            digits = function(value) {

              if(missing(value)) return(private$digits_pvt)

              private$digits_pvt <- value

            },

            stats = function(value) {

              if(missing(value)) return(private$stats_pvt)

              private$stats_pvt <- value

            }

          )
  )
