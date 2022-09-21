
#' Modules Used by Geography
#'#'
#' @return data frame of module information
#' @export
#'
#' @examples
#' \dontrun{
#' modules_used()
#' }
#'
#'
modules_used <- function() {

  # year = get.year(year)
  # extent = get.extent(extent)
  # source = get.source(source)

  codebook <- get.codebook.layout()

  brfss.param(version = 0)
  df_brfss <- brfss_data()

  if(is.null(codebook) || is.null(df_brfss)) return(NULL)

  df_modules <- codebook %>%
    filter(sect_type == "Module") %>%
    filter(!grepl("^Calc|^Questionnaire", section)) %>%
    filter(!grepl("^_", col_name)) %>%
    select(col_name, sect_type, sect_num, question_num, section) %>%
    group_by(sect_num) %>%
    filter(row_number() == 1) %>%
    as.data.frame()


  df_final <- data.frame()

  sapply(0:highest_version(),function(ver){

    brfss.param(version = ver)
    df_brfss <- brfss_data()

    df_brfss <- df_brfss %>%
      mutate(geog = `_STATE`)

    invisible(
      mapply(function(coi, mod_num, module) {
        # if(ver == 1 && mod_num == 2) browser()
        #cat("coi: ", coi, "\n")
        df <- df_brfss %>%
          rename(coi = {{coi}}) %>%
          select(geog,coi) %>%
          filter(!is.na(coi)) %>%
          select(geog) %>%
          distinct() %>%
          mutate(year = brfss.param(year), version = brfss.param(version),
                 mod_num = mod_num, module = module) %>%
          select(year, version, geog, mod_num, module)


        df_final <<- df_final %>% bind_rows(df)

      },df_modules$col_name,df_modules$sect_num, df_modules$section)
    )

  })

  df_final %>% mutate(geog = geog_abb(geog))

}

save_module_stats<-function() {
  require(dplyr)

  params <- my.brfss.patterns()

  df_responses<-responses()

  df_mods<-modules_used()


  df_modules<-dplyr::left_join(df_mods,df_responses,by = c("year", "geog", "version"))

  df_mods_tots<-aggregate(responses ~ year + geog + mod_num, data=df_modules,FUN=sum)

  df_modules<-dplyr::left_join(df_modules,df_mods_tots,by = c("year", "geog", "mod_num"))

  colnames(df_modules)<-gsub("[.]x","",colnames(df_modules))
  colnames(df_modules)<-gsub("[.]y","_total",colnames(df_modules))

  df_modules$ratio<-df_modules$responses/df_modules$responses_total

  saveRDS(df_modules, file = apply.pattern("brfss_modules_path",params))

}


#' Get BRFSS Modules Used by State(s)
#'
#' Returns  a data frame that has the year, the version (if requested), the geog (2-char), and the module used or returns a
#' vector of modules if appropriate
#'
#' @param year - integer - 4-digit year of interest
#' @param geogs - character or integer vector - geogs of interest - can be 2 character geog code or FIPS code (integer)
#' @param versions - logical - include the version number
#' @param reduce - logical - reduce to a vector if appropriate
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' geog_modules(2018,"WI")
#' geog_modules(2018,c(1,2))
#' geog_modules(2018,"MI",versions=T)
#'}
#'

geog_modules<-function(year = NULL,geogs = NULL, versions=FALSE,reduce=TRUE) {

  my_brf <- brfss.params()

  if(is.null(year)) year <- my_brf["year"]

  if(is.null(geogs)) {
    geogs <- unique(c(my_brf["geog"], my_brf["geogs_other"]))
  }

  df<- module_data(year)

  if(!is.null(geogs)) {
    #   if(is.numeric(geogs)) geogs<-geog_abbs(geogs)
    if(is.numeric(geogs)) geogs<-geog_abb(geogs)
    df<-df[df$geog%in%geogs,]
  } else {
    geogs<-geog_abbs(geogs)
  }

  if(reduce && length(geogs)==1 && !versions) {
    return(unique(df$module))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"geog","module")]))

  }
}

#' Get States Using BRFSS Module(s)
#'
#' Returns  a data frame that has the year, the version (if requested), the geog (2-char), and the module used or returns a
#' vector of modules if appropriate
#'
#' @param year - integer - 4-digit year of interest
#' @param modules - character or integer vector - modules of interest - can be module numbers or module names
#' @param versions - logical - include the version number
#' @param reduce - logical - reduce to a vector if appropriate
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' module_geogs(2018,"Pre-Diabetes")
#' module_geogs(2018,c(1,2))
#'}
#'
module_geogs<-function(year = NULL,modules,versions=FALSE,reduce=TRUE) {

  year <- get.year(year)

  df<- module_data(year)

  if(is.character(modules)) col<-"module" else col<-"mod_num"
  df<-df[df[[col]]%in%modules,]

  if(reduce && length(modules)==1 && !versions) {
    return(unique(df$geog))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"geog","module")]))
  }
}

#' Module Data
#'
#' @param year integer: year of interest
#'
#' @return data frame: modules by state
#' @export
#'
#' @examples
#' \dontrun{
#' df <- module_data(2021)
#' }
module_data<-function(year) {

  readRDS(orrr::dir.project(c("data",year,paste0("modules_",year,".rda")),slash = F))
}
