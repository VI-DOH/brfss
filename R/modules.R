
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

# modules_used<-function(year = NULL, geogs = NULL, verbose = FALSE) {
#
#   year <- get.year(year)
#
#   vers_max<-highest_version(year)
#
#   if(is.null(geogs)) geogs<-get_geogs_all() %>% pull(Abbrev)
#
#   df<-data.frame()
#
#
#   sapply(geogs, function(geog){
#     sapply(0:vers_max,function(ver){
#       if(verbose) cat(paste0(" modules ... trying ", geog, "_V",ver,"\n"))
#       df<<-rbind(df,calc_modules_by_geog(year,geog,version = ver))
#     })
#   })
#
#   df
#
# }

# calc_modules_by_geog<-function(year,geog,version=0) {
#
#   ##
#   ##    change numeric geog (fips) to abbrev
#
#   if(is.numeric(geog)) geog<-geog_abbs(geog)
#
#   ##
#   ##    if the version exists for that geog
#
#   if(brfss_version_exists(year,geog,version)) {
#
#     ##
#     ##    get the data for that year/geog/version
#
#     id <- geog_id(geog)
#
#     df0<-brfss_data(year,geog,version) %>%
#       filter(`_STATE` == id)
#
#     if(nrow(df0)>0) {
#
#
#       df_columns<-data.frame()
#       invisible(
#         sapply(colnames(df0), function(col) {
#           ##
#           ##    get the attributes
#
#           att<-attributes(df0[[col]])
#
#           if(length(att)==6) {
#             section_type<-att["section_type"]
#             section_num<-att["section_num"]
#             section_index<-att["section_index"]
#             section_name<-att["section_name"]
#             label<-att["label"]
#
#             df<-data.frame(column=col,
#                            section_type,
#                            section_num,
#                            section_index,
#                            section_name,
#                            label)
#
#             df_columns<<-rbind(df_columns,df)
#           }
#         })
#       )
#
#       df_module_chkr<-df_columns[grepl("Mod",df_columns$section_type) &
#                                    df_columns$section_index==1 &
#                                    !grepl("^Calc",df_columns$section_name)&
#                                    !grepl("^Questionnaire",df_columns$section_name),
#                                  c("column","section_num", "section_index","section_name")]
#
#       df_module_chkr$used<-sapply(df_module_chkr$column,function(col) {
#         nrow(table(df0[[col]]))>0
#       })
#
#       df_module_chkr$geog<-geog
#       df_module_chkr$year<-year
#       df_module_chkr$version<-version
#
#       return(df_module_chkr[df_module_chkr$used,c("year","version","geog","section_num","section_name")])
#     }
#   }
#   return(data.frame(year=integer(0),
#                     version=integer(0),
#                     geog=character(0),
#                     section_num=character(0),
#                     section_name=character(0)))
# }


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


  nm<- apply.pattern("brfss_modules_df", params)
  assign(nm,df_modules)

  save(list = c(nm),file = apply.pattern("brfss_modules_path",params))

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

  year <- get.year(year = year)

  if(is.null(geogs)) {
    geogs <- c(my.geog(), my.other.geogs())
  }

  df<- module_data(year)

  if(!is.null(geogs)) {
    if(is.numeric(geogs)) geogs<-geog_abbs(geogs)
    df<-df[df$geog%in%geogs,]
  } else {
    geogs<-geog_abbs()
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

  orrr::get.rdata(orrr::dir.project(c("data",year,paste0("modules_",year,".rda")),slash = F))
}
