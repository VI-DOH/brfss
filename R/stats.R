


brfss.data<-function(year,states,version=0) {

  # year<-2019
  #
  # state<-"MT"
  #

  df<-data.frame()

  sapply(states,function(state) {
    rdata_file<-brfss_geog_data_filename(year,state,version)
    df<<-rbind(df,orrr::get.rdata(file = rdata_file))
  })

  df


}

# var<-"DIABETE4"
# ignore<-c(7,9)
# vals<-c(1)
# num_vals <- 1
# den_vals <- c(1:4)
#
# subsets<- c("SEXVAR")
#
#
# df<-df0[,c("_LLCPWT",var,subsets)]
# table(df[,var])
# colnames(df)<-c("WT","VAR",subsets)
# survey_stats_binary(df0 = df,coi = "VAR",subset=subsets,num_vals = num_vals,den_vals = den_vals, weighting="WT")
