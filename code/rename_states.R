
e<-new.env()
load(file = "./R/sysdata.rda",envir = e)
df_states<-get("states",envir = e)
fldr_data<-"./data/"

sapply(df_states$Abbrev,function(abb){
 # file.rename(from=paste0(fldr_data,abb,".RData"), to=paste0(fldr_data,"brfss_",abb,"_2016.RData"))
  file.rename(from=paste0(fldr_data,"btfss_",abb,"_2016.RData"), to=paste0(fldr_data,"brfss_",abb,"_2016.RData"))

})

rm(e)
rm(df_states)
