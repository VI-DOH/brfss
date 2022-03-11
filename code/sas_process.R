require(haven)
require(dplyr)


read <- F


if(read) df_sas_in <- read_xpt("./data_raw/test/LLCP2020.XPT") %>%
  filter(`_STATE` == 16)

df_sas <- df_sas_in %>%
  mutate(IMONTH = as.character(as.integer(IMONTH)))
############################################
##
##    read Formas file

lines <- readLines(con = "./data_raw/test/Formas20.sas")
lines <- gsub(" FORMAT ","",lines)
lines <- grep(";",lines,value = T,invert = T)
varout <- gsub(" *([^ ]*) *(.*)","\\1",lines)
varin <- gsub(" *([^ ]*) *([^ .]*)","\\2",lines)

############################
##
##  read format file

lines <- readLines(con = "./data_raw/test/Format20.sas")

lines <- grep("^( *[.]|PROC|run)",lines,value = TRUE, invert = T)
lines <- gsub("\"","",lines)
lines <- gsub("’","'",lines)

invisible(
  mapply(function(vin,vout) {
    #browser()
    lines <<- gsub(vin,vout,lines,fixed = T)

  }, varin, varout)
)


val_lines <- grep(" *VALUE ",lines)

stop_lines <- grep(";",lines)

cnames <- colnames(df_sas)

invisible(
  mapply(function(strt,end) {
    var <- stringr::str_trim(gsub(" *VALUE (.*)","\\1",lines[strt]))

    if(var %in%cnames){
      var_lines <- lines[(strt+1):end]
      levels <- stringr::str_trim(gsub("(.*?)=(.*)","\\1",var_lines))
      labels <- stringr::str_trim(gsub("(.*?)=(.*)","\\2",var_lines))
       cat(var, " | ")

       #if(var == "_METSTAT") browser()

      if(!any(grepl("-",levels))) {
        if(is.numeric(df_sas[[var]])) levels<- as.integer(levels)
        # df_sas <<- df_sas %>%
        #   mutate({{var}} := factor({{var}},levels = levels, labels = labels))
        df_sas[var] <<- factor(df_sas[[var]],levels = levels, labels = labels)
      }  else {

      }

    }

  },val_lines, stop_lines-1)

)


str(df_sas)

table(df_sas$IMONTH)

