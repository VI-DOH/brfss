
##########################################################################################################
##
##
##
##
#########################################################################################################

center_string<-function(x,len_out,ctr_char) {
  if(missing(len_out)) len_out<-nchar(x)
  x<-trimws(x)

  len_in<-nchar(x)

  if(len_in>len_out) {
    ret<-substr(x,1,len_out)
  } else {
    if(missing(ctr_char)){
      spc1<-as.integer((len_out-len_in)/2)
      spc2<-len_out-len_in-spc1
    } else {
      pos = regexpr(ctr_char,x)
      if(pos>0) {
        spc1<-as.integer(len_out/2) - pos
        spc2<-len_out-len_in-spc1
      }
    }
    ret<-paste(spaces(spc1),x,spaces(spc2),sep="")
  }


  ret
}


newlines<-function(n) {
  stringr::str_dup(string = "\n",times = n)
}


spaces<-function(n) {
  stringr::str_dup(string=" ",times=n)
}

