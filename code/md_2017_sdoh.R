
st<-"OH"
year<-2017
version<-0

fname<-paste0(orrr::dir.project("data"),year,"/brfss_",st,"_",year,".RData")
if(version>0) fname<-gsub("[.]RData",paste0("_V",version,".RData"),fname)



df0<- orrr::get.rdata(fname)

version<-1

fname<-paste0(orrr::dir.project("data"),year,"/brfss_",st,"_",year,".RData")
if(version>0) fname<-gsub("[.]RData",paste0("_V",version,".RData"),fname)
df1<- orrr::get.rdata(fname)


version<-2

fname<-paste0(orrr::dir.project("data"),year,"/brfss_",st,"_",year,".RData")
if(version>0) fname<-gsub("[.]RData",paste0("_V",version,".RData"),fname)
df2<- orrr::get.rdata(fname)

col<-"SDHMONEY"

table(df2$SDHMONEY)

df_columns<-data.frame()

invisible(
  sapply(colnames(df0), function(col) {

    att<-attributes(df0[[col]])
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
  })
)

df_module_chkr<-df_columns[grepl("Mod",df_columns$section_type) & df_columns$section_index==1,
                           c("column","section_num", "section_index")]

df_module_chkr

modules<-sapply(df_module_chkr$column[1:10],function(col) {
  nrow(table(df0[[col]]))>0
})
