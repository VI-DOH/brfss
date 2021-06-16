highest_version<-function(year) {
  fldr<-brfss_data_folder(year)

  files<-list.files(fldr)
  files<-files[grep("_V[0-9][.]",files)]
  vers<-as.integer(gsub(".*_V([0-9]*)[.].*","\\1",files))
  max(vers)
}

modules_used<-function(year,states) {

  vers_max<-highest_version(year)

  if(missing(states)) states<-state.abb

  df<-data.frame()


  sapply(states, function(state){
    sapply(0:vers_max,function(ver){
      df<<-rbind(df,modules_used_by_state(year,state,version = ver))
    })
  })

  df

}

modules_used_by_state<-function(year,state,version=0) {
  abb<-state_abbs(state)
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

