

fname<-"./data_raw/2017/ascii/LLCP2017ASC/LLCP2017.ASC"

split.ascii<-function(fname) {

  txt<-readLines(fname)


  state<-sapply(txt,function(ln) {
    substring(text = ln,first = 1,last=2)
  },USE.NAMES = F)

  table(state)

  states<-unique(state)

  df<-data.frame(stringsAsFactors = F)
  invisible(
    sapply(states,function(st) {
      w<-which(state==st)
      df<<-rbind(df,data.frame(state=as.integer(st),min=min(w),max=max(w),stringsAsFactors = F))

    })
  )
  df_states<-read.csv("./data_raw/states.csv")

  df_states<- dplyr::inner_join(x = df_states,y=df,by=c("Id"="state"))

  invisible(
    mapply(function(st,mn,mx) {
      lines<-txt[mn:mx]
      writeLines(text = lines , con = paste0("./data/2017/ascii/",st,".asc"))
    },df_states$Abbrev,df_states$min,df_states$max)
  )

}


