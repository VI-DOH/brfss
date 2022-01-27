
modules_used<-function(year,states) {

  vers_max<-highest_version(year)

  if(missing(states)) states<-state_abbs()

  df<-data.frame()


  sapply(states, function(state){
    sapply(0:vers_max,function(ver){
      df<<-rbind(df,calc_modules_by_state(year,state,version = ver))
    })
  })

  df

}

calc_modules_by_state<-function(year,state,version=0) {

  if(is.numeric(state)) state<-state_abbs(state)

  if(brfss_state_version_exists(year,state,version)) {

    df0<-brfss_state_data(year,state,version)


    df_columns<-data.frame()
    invisible(
      sapply(colnames(df0), function(col) {
        att<-attributes(df0[[col]])
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

    df_module_chkr$state<-state
    df_module_chkr$year<-year
    df_module_chkr$version<-version

    return(df_module_chkr[df_module_chkr$used,c("year","version","state","section_num","section_name")])
  } else {
    return(data.frame(year=integer(0),
                      version=integer(0),
                      state=character(0),
                      section_num=character(0),
                      section_name=character(0)))
  }

}

save_module_stats<-function(year) {
  require(dplyr)

  df_responses<-responses(year = year)

  df_mods<-modules_used(year = year)

  df_modules<-dplyr::left_join(df_mods,df_responses,by = c("year", "state", "version"))


  df_mods_tots<-aggregate(responses ~ year + state + section_num, data=df_modules,FUN=sum)

  df_modules<-dplyr::left_join(df_modules,df_mods_tots,by = c("year", "state", "section_num"))

  colnames(df_modules)<-gsub("[.]x","",colnames(df_modules))
  colnames(df_modules)<-gsub("[.]y","_total",colnames(df_modules))

  df_modules$ratio<-df_modules$responses/df_modules$responses_total
  nm<-paste0("df_responses_",year)
  assign(nm,df_responses)
  save(list = c(nm),file = paste0(brfss_data_folder(year = year),"responses_",year,".RData"))

  nm<-paste0("df_modules_",year)
  assign(nm,df_modules)
  save(list = c(nm),file =  paste0(brfss_data_folder(year = year),"modules_",year,".RData"))
}


#' Get BRFSS Modules Used by State(s)
#'
#' Returns  a data frame that has the year, the version (if requested), the state (2-char), and the module used or returns a
#' vector of modules if appropriate
#'
#' @param year - integer - 4-digit year of interest
#' @param states - character or integer vector - states of interest - can be 2 character state code or FIPS code (integer)
#' @param versions - logical - include the version number
#' @param reduce - logical - reduce to a vector if appropriate
#' @return
#' @export
#'
#' @examples
#' state_modules(2018,"WI")
#' state_modules(2018,c(1,2))
#' state_modules(2018,"MI",versions=T)

state_modules<-function(year,states,versions=FALSE,reduce=TRUE) {

  df<- module_data(year)

  if(!missing(states)) {
    if(is.numeric(states)) states<-state_abbs(states)
    df<-df[df$state%in%states,]
  } else {
    states<-state_abbs()
  }

  if(reduce && length(states)==1 && !versions) {
    return(unique(df$section_name))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"state","section_name")]))

  }
}

#' Get States Using BRFSS Module(s)
#'
#' Returns  a data frame that has the year, the version (if requested), the state (2-char), and the module used or returns a
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
#' module_states(2018,"Pre-Diabetes")
#' module_states(2018,c(1,2))
#'
module_states<-function(year,modules,versions=FALSE,reduce=TRUE) {

  df<- module_data(year)

  if(is.character(modules)) col<-"section_name" else col<-"section_num"
  df<-df[df[[col]]%in%modules,]

  if(reduce && length(modules)==1 && !versions) {
    return(unique(df$state))
  } else {
    if(versions) vcol<-"version" else vcol=NULL
    return(unique(df[,c("year",vcol,"state","section_name")]))
  }
}

module_data<-function(year) {

  orrr::get.rdata(orrr::dir.project(c("data",year,paste0("modules_",year,".RData")),slash = F))
}
