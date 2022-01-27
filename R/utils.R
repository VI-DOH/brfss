
state_ids<-function(states) {

  ##  get data.frame of states

  df_states<- orrr::get.rdata(paste0(orrr::dir.project("data"),"states.RData"))

  if(is.character(states)) {

    if(nchar(states[1])==2) {
      states<-sapply(states,function(state) {
        df_states[df_states$Abbrev==state,"Id"]
      })
    } else {
      states<-sapply(states,function(state) {
        df_states[df_states$State==state,"Id"]

      })
    }
  } else {
    return(states)
  }
  states
}


state_abbs<-function(states) {

  ##  get data.frame of states

  df_states<- orrr::get.rdata(paste0(orrr::dir.project("data"),"states.RData"))

  if(missing(states)) {
    states<-df_states$Abbrev
  } else {
    if(is.character(states)) {

      if(nchar(states[1])==2) {
        return(states)
      } else {
        states<-sapply(states,function(state) {
          df_states[df_states$State==state,"Abbrev"]

        })
      }
    } else {
      states<-sapply(states,function(state) {
        df_states[df_states$Id==state,"Abbrev"]

      })  }

  }
  states
}


state_names<-function(states) {

  ##  get data.frame of states

  df_states<- orrr::get.rdata(paste0(orrr::dir.project("data"),"states.RData"))

  if(is.character(states)) {

    if(nchar(states[1])!=2) {
      return(states)
    } else {
      states<-sapply(states,function(state) {
        df_states[df_states$Abbrev==state,"State"]

      })
    }
  } else {
    states<-sapply(states,function(state) {
      df_states[df_states$Id==state,"State"]

    })  }
  states
}

brfss_state_data<-function(year,state,version=0) {
  if(is.numeric(state)) state<-state_abbs(state)
  fname<-brfss_state_data_filename(year,state,version=version)
  if(file.exists(fname)) {
    df_brfss<- orrr::get.rdata(fname)
  } else {
    df_brfss<-NULL
  }

  df_brfss
}
