
#' Modules Used by Geography
#'
#' @param year integer - year of interest
#' @param geogs character - geography(s) of interest
#'
#' @return data frame of module information
#' @export
#'
#' @examples
#' \dontrun{
#' modules_used(2020, "MT")
#' }
#'
modules_used<-function(year = NULL, geogs = NULL) {

  year <- get.year(year)

  vers_max<-highest_version(year)

  if(is.null(geogs)) geogs<-get.geogs.all() %>% pull(Abbrev)

  df<-data.frame()


  sapply(geogs, function(geog){
    sapply(0:vers_max,function(ver){
      df<<-rbind(df,calc_modules_by_geog(year,geog,version = ver))
    })
  })

  df

}

calc_modules_by_geog<-function(year,geog,version=0) {

  if(is.numeric(geog)) geog<-geog_abbs(geog)

  if(brfss_geog_version_exists(year,geog,version)) {

    df0<-brfss_data(year,geog,version)


    df_columns<-data.frame()
    invisible(
      sapply(colnames(df0), function(col) {
        att<-attributes(df0[[col]])
        browser()
        if(length(att)==5) {
          section_type<-att["section_type"]
          section_num<-att["section_num"]
          section_index<-att["section_index"]
          section_name<-att["section_name"]
          label<-att["label"]

          df<-data.frame(column=col,
                         section_type,
                         section_num,
                         section_index,
                         section_name,
                         label)
          df_columns<<-rbind(df_columns,df)
        }
      })
    )

    df_module_chkr<-df_columns[grepl("Mod",df_columns$section_type) & df_columns$section_index==1,
                               c("column","section_num", "section_index","section_name")]

    df_module_chkr$used<-sapply(df_module_chkr$column,function(col) {
      nrow(table(df0[[col]]))>0
    })

    df_module_chkr$geog<-geog
    df_module_chkr$year<-year
    df_module_chkr$version<-version

    return(df_module_chkr[df_module_chkr$used,c("year","version","geog","section_num","section_name")])
  } else {
    return(data.frame(year=integer(0),
                      version=integer(0),
                      geog=character(0),
                      section_num=character(0),
                      section_name=character(0)))
  }

}

save_module_stats<-function(year) {
  require(dplyr)

  df_responses<-responses(year = year)

  df_mods<-modules_used(year = year)


  df_modules<-dplyr::left_join(df_mods,df_responses,by = c("year", "geog", "version"))

  df_mods_tots<-aggregate(responses ~ year + geog + section_num, data=df_modules,FUN=sum)

  df_modules<-dplyr::left_join(df_modules,df_mods_tots,by = c("year", "geog", "section_num"))

  colnames(df_modules)<-gsub("[.]x","",colnames(df_modules))
  colnames(df_modules)<-gsub("[.]y","_total",colnames(df_modules))

  df_modules$ratio<-df_modules$responses/df_modules$responses_total
  # nm<-paste0("df_responses_",year)
  # assign(nm,df_responses)
  # save(list = c(nm),file = paste0(apply.pattern("sas_data_folder", YEAR = year),"responses_",year,".RData"))

  nm<-paste0("df_modules_",year)
  assign(nm,df_modules)

  save(list = c(nm),file = apply.pattern("brfss_modules_path",YEAR = year))

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
    return(unique(df$section_name))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"geog","section_name")]))

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

  if(is.character(modules)) col<-"section_name" else col<-"section_num"
  df<-df[df[[col]]%in%modules,]

  if(reduce && length(modules)==1 && !versions) {
    return(unique(df$geog))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"geog","section_name")]))
  }
}

module_data<-function(year) {

  orrr::get.rdata(orrr::dir.project(c("data",year,paste0("modules_",year,".RData")),slash = F))
}
