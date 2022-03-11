

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

get.layout<-function(fname="./data_raw/2017/layout/layout.csv") {
  df_layout<-read.csv(fname,stringsAsFactors = F)
  colnames(df_layout)<-c("start","varname","length")
  df_layout$end<-df_layout$start+df_layout$length-1
  which(grepl("^X_",df_layout$varname))
  df_layout$varname<-gsub("^_","X_",df_layout$varname)
  df_layout$nextvar<-df_layout$start+df_layout$length

  df_alias<-data.frame(stringsAsFactors = FALSE)
  invisible(sapply(2:nrow(df_layout),function(i){

    if (df_layout[i,"start"]==df_layout[i-1,"start"] && df_layout[i,"length"]==df_layout[i-1,"length"]) {
      df_alias<<-rbind(df_alias,data.frame(x=df_layout[i,"varname"],y=df_layout[i-1,"varname"],stringsAsFactors = F))
    }

  }))

  df_covered<-data.frame(stringsAsFactors = FALSE)

  del<-which(as.logical(sapply(1:nrow(df_layout),function(i){

    rm<-(
      (i<nrow(df_layout) && df_layout[i,"start"]==df_layout[i+1,"start"] && df_layout[i,"length"]>df_layout[i+1,"length"])
      ||
        (i>1 && df_layout[i,"start"]==df_layout[i-1,"start"] && df_layout[i,"length"]>df_layout[i-1,"length"])
    )

    if(rm) df_covered<<-rbind(df_covered,df_layout[i,])

    rm<-rm || (df_layout[i,"start"]==df_layout[i-1,"start"] && df_layout[i,"length"]==df_layout[i-1,"length"])

    return(rm)
  })))
  df_layout<-df_layout[-del,]

  addstart<-df_layout$nextvar[which(!df_layout$nextvar%in%df_layout$start)]
  addstart<-addstart[addstart<max(df_layout$start)]
  addend<-sapply(addstart,function(st) {
    x<-df_layout$start[df_layout$start>st]
    x<-min(x)
    x<-x-1
    x
  })

  invisible(
    mapply(function(st,end) {
      df_layout<<-rbind(df_layout,data.frame(start= st,varname=paste0("DUMMY_",st), length = end-st+1, end=end, nextvar=end+1))
    },addstart,addend)
  )

  df_layout[order(df_layout$start),c("varname", "start","end","length")]

}
