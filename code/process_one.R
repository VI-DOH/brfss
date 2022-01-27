
year<-2019

state<-"MT"

rdata_file<-paste0(orrr::dir.project("data"),year,"/brfss_",state,"_",year,".RData")

df0<- orrr::get.rdata(file = rdata_file)


var<-"DIABETE4"
ignore<-c(7,9)
vals<-c(1)
num_vals <- 1
den_vals <- c(1:4)

subsets<- c("SEXVAR")


df<-df0[,c("_LLCPWT",var,subsets)]
table(df[,var])
colnames(df)<-c("WT","VAR",subsets)
survey_stats_binary(df0 = df,coi = "VAR",subset=subsets,num_vals = num_vals,den_vals = den_vals, weighting="WT")

