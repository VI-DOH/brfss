
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

split_sentence<-function(x,len, start=0){
  #
  # find spaces
  #
  lenx<-nchar(x)

  sp_all<-gregexpr(pattern = " ",text = x)

  if(length(sp_all)>0) {

    sp_all<-as.integer(sp_all[[1]])

    # get the spaces after the start of the search (start) and
    # before the length of interest for each line (start + len)
    #
    sp<-sp_all[sp_all<(start+len) & sp_all>start]

    # see if there are any matches
    if(length(sp)>0 && (start+len)<lenx) {

      # insert new line (\n) at the correct space (largest in this group)
      #
      nl<-max(sp)

      #     ptrn<-paste("(.{",nl-1,"}) (.*)",sep="")
      #     x<-gsub(ptrn,"\\1\n\\2",x)
      x<-paste(substring(x,1,nl-1),substring(x,nl+1),sep="\n")
      #
      # see if there are any more
      #
      if(nl<sp_all[length(sp_all)]) {
        x<-split_sentence(x,len,nl)
      }
    }
  }
  x
}
