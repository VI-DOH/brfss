


df_mt_15<- orrr::get.rdata("./data/2015/brfss_MT_2015.RData")
df_mt_16<- orrr::get.rdata("./data/2016/brfss_MT_2016.RData")
df_mt_17<- orrr::get.rdata("./data/2017/brfss_MT_2017.RData")
df_mt_18<- orrr::get.rdata("./data/2018/brfss_MT_2018.RData")

e<-environment()
invisible(
  sapply(15:18,function(yr) {
    df<-get(paste0("df_mt_",yr))
    str(df[,"SEQNO"])
    df[,"SEQNO"]<-as.integer(df[,"SEQNO"])
    assign(df,paste0("df_mt_",yr),envir = e)
  })
)

str(df_mt_15$SEQNO)
str(df_mt_16$SEQNO)
str(df_mt_17$SEQNO)
str(df_mt_18$SEQNO)

df_mt_15$SEQNO<-as.integer(df_mt_15$SEQNO)
df_mt_16$SEQNO<-as.integer(df_mt_16$SEQNO)
df_mt_17$SEQNO<-as.integer(df_mt_17$SEQNO)
df_mt_18$SEQNO<-as.integer(df_mt_18$SEQNO)

df_mt<-dplyr::bind_rows(df_mt_15,df_mt_16,df_mt_17,df_mt_18)
df_mt$YEAR<-as.integer(substr(df_mt$SEQNO,4))


table(df_mt$IYEAR,df_mt$DIABETE3)

year<-2018

var<-"DIABETE3"
ignore<-c(7,9)
vals<-c(1)

df<-df_mt[df_mt$IYEAR==year,c(var)]
df<-df_mt_18[df_mt_18$IYEAR==year,c(var)]

tbl<-table(df)
tbl_ign<-as.integer(names(tbl))%in%ignore

tbl<-tbl[!tbl_ign]
tbl_vals<-which(as.integer(names(tbl))%in%vals)
tbl<-addmargins(tbl)

n<-sum(tbl[tbl_vals])

##########################################################################################
##
##

year<-2016

state<-"MT"

rdata_file<-paste0(orrr::dir.project("data"),year,"/brfss_",state,"_",year,".RData")

df0<- orrr::get.rdata(file = rdata_file)


var<-"DIABETE3"

ignore<-c(7,9)
vals<-c(1)
num_vals <- 1
den_vals <- c(1:4)

df<-df0[,c("_LLCPWT",var)]
table(df[,var])
colnames(df)<-c("WT","VAR")
survey_stats_binary(df0 = df,coi = "VAR",num_vals = num_vals,den_vals = den_vals, weighting="WT")


