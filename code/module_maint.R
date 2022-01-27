

save_module_stats(2012)
save_module_stats(2013)
save_module_stats(2014)
save_module_stats(2015)
save_module_stats(2016)
save_module_stats(2017)
save_module_stats(2018)
save_module_stats(2019)


source("./R/stats.R")

year<-2018
state<-"MI"

use_ratio<-FALSE

df0<-brfss.data(year,states = state,0)
df2<-brfss.data(year,states = state,version = 2)
df3<-brfss.data(year,states = state,version = 3)

cnames0<-colnames(df0)
cnames2<-colnames(df2)
cnames3<-colnames(df3)
cnames0[!cnames0%in%cnames2]
cnames2[!cnames2%in%cnames0]
cnames3[!cnames3%in%cnames0]


df_resp<-responses(year,states=state)

if(use_ratio) {
  tot_resp<-sum(df_resp$responses)

  ratio0<-df_resp[df_resp$version==0,"responses"]/tot_resp    #df0$'_LLCPWT'
  ratio2<-df_resp[df_resp$version==2,"responses"]/tot_resp    #df0$'_LLCPWT'
  ratio3<-df_resp[df_resp$version==3,"responses"]/tot_resp    #df0$'_LLCPWT'

  df0$FINAL_WT<-df0$'_LLCPWT'*ratio0
  df2$FINAL_WT<-df2$'_LCPWTV2'*ratio2
  df3$FINAL_WT<-df3$'_LCPWTV3'*ratio3
} else {

  df0$FINAL_WT<-df0$'_LLCPWT'
  df2$FINAL_WT<-df2$'_LCPWTV2'
  df3$FINAL_WT<-df3$'_LCPWTV3'

}
df0$'_LLCPWT'<-df0$'_CLLCPWT'<-NULL
df2$'_LCPWTV2'<-df2$'_CLCPWTV2'<-NULL
df3$'_LCPWTV3'<-df3$'_CLCPWTV3'<-NULL


var<-"DIABETE3"

denom<-c(1:4)


cols<-c(var,"FINAL_WT","_STSTR")

brfss<-rbind(df0[cols],df2[cols],df3[cols])
brfss<-df0[cols]

brfss<-brfss[brfss[[var]]%in%denom,]
colnames(brfss)[colnames(brfss)=="_STSTR"]<-"STSTR"
require(survey)

# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust")
# Create survey design
brfssdsgn <- svydesign(
  id=~1,
  strata = ~STSTR,
  weights = ~FINAL_WT,
  data = brfss)
# calculate average number of physical healthy days
# svymean(~GENHLTH, # Variable to anlayze
#         brfssdsgn,
#         na.rm = TRUE)
# calculate percent in each arthritis category

df_stats<-as.data.frame(svymean(~factor(DIABETE3),
        brfssdsgn, na.rm = TRUE))

df_stats$ll<-df_stats$mean-df_stats$SE
df_stats$ul<-df_stats$mean+df_stats$SE

df_stats$mean<-round(df_stats$mean,4)*100
df_stats$SE<-round(df_stats$SE,4)*100
df_stats$ll<-round(df_stats$ll,4)*100
df_stats$ul<-round(df_stats$ul,4)*100

df_stats

table(brfss$DIABETE3)

