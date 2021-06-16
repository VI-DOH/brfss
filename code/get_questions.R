
sas.build.desc<-function(year) {

  file<-paste0("./data_raw/",year,"/sas/SASOUT",year%%100,"_LLCP.SAS")

  lines<-readLines(file)


  lines<-grep(" = '",lines,value = T)

  var<-gsub("(.*) = .*","\\1",lines)
  desc<-gsub("(.*) = '(.*)'","\\2",lines)

  df_name<-paste0("df_desc_",year)
  rdata_name<-paste0(orrr::dir.project("data"),year,"/desc_",year,".RData")
  assign(df_name,data.frame(var,desc))

  save(list = c(df_name),file = rdata_name)
}
