

#' Get BRFSS Fixed Width Layout
#'
#'Gets a data frame with information about the column names and the column widths for parsing a
#'BRFSS fixed width ASCII file.
#'
#' @param year integer - year of interest
#'
#' @return data frame- 2 variables
#' @export
#'
#' @examples
#' \dontrun{
#' get_fw_layout(2020)
#' }
get_fw_layout <- function(year = NULL) {

  year <- get.year(year)

  layout_fldr <- apply.pattern("sas_layout_folder", YEAR = year)

  list.files(layout_fldr)
}

sas_layout<-function(year){

  #file= paste0(orrr::dir.project("data_raw"),year,"/SASOUT",year%%100,"_STATES.SAS")
  read.sas.layout(year = year)
}


fixed_width_layout<-function(year) {

  df_saq<- saq_data(year)
  # get sas layout provided by CDC

  df_sas_layout<-sas_layout(year)

  # clean up sas layout - remove overlapping vars
  #   and remove the state-added-question columns which are to be added from df_saq

  df_layout<-df_sas_layout[!df_sas_layout$col_name%in%c('IDATE','X_PSU',"STATEQUE"),]

  df_layout<-rbind(df_layout[,c("start","end","col_name","field_size","label")],
                   df_saq[,c("start","end","col_name","field_size","label")])

  # build ordered vectors for start and end with no gaps
  #   and compute widths for read.fwf
  #   gaps are replaced with -widths to ignore those columns

  df_layout$start<-as.integer(df_layout$start)
  df_layout$end<-as.integer(df_layout$end)

  df_layout<-df_layout[order(df_layout$start),]

  st<-df_layout$start
  end<-df_layout$end

  # calculate nxt ... the starts of the following items
  nxt<-c(st[-1],st[length(st)]+1)

  # here is where we fix the gaps ...
  #  if there is not data in the column(s) following those data, then we add a gap
  #   fix is a vector of items that lack data in the next column,
  #   i.e. the next data starts in column x which is not 1 column after the end of my data

  fix<-which(nxt!=(end+1))
  add_st<-end[fix]+1
  add_end<-st[fix+1]-1

  # combine the columns data with the added gaps and reorder by start column

  df_fwf<-rbind(df_layout[,c("start","end","field_size")],
                data.frame(start=add_st,end=add_end,field_size=-(add_end-add_st+1)))

  df_fwf<-df_fwf[order(df_fwf$start),]
  list(widths=df_fwf$field_size,cols=df_layout$col_name,layout=df_layout)
}


merge_layout<-function(df_quest, year = NULL) {

  year <- get.year(year)

  df_layout<-sas_layout(year)

  df_quest<- df_quest %>%
    mutate(index = as.integer(qnumber)) %>%
    rename(type_index = snumber)

  df_questions<-dplyr::left_join(df_quest,df_layout) %>%
    select(qnum, question, type, section, type_index, code, index,label, col_name)

}
