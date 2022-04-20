require(dplyr)


#' Split BRFSS Data by Geography
#'
#' The main BRFSS  data file created when the file is downloaded and read has data
#' for all geographies, whether the SAS (XPT) file or the ASCII (ASC) file. This function splits out the geographies of interest.
#'
#' @param year integer - year of interest
#' @param source character - data source ('sas' or 'ascii')
#' @param main logical - process main XPT file
#' @param versions logical - process versioned XPT file
#' @param my_geog character - abbreviation for primary state/geography of interest (e. "MT")
#' @param other_geogs character - abbreviations for other states/geographies of interest (e. c("ID","WY"))
#' @param verbose logical - provide details during processing
#'
#' @export
#' @examples
#'
#'\dontrun{
#' split_geogs(year = 2020, source = 'sas', main=TRUE, versions=TRUE, my_geog="MT", other_geogs=NULL,verbose=TRUE)
#'}
#'
#'
split_geogs<-function(year = NULL, source = NULL,
                      main=TRUE, versions=TRUE, my_geog=NULL,
                      other_geogs=NULL,verbose=TRUE) {

  if(!(main || versions)) return(NULL)

  year <- get.year(year)
  source <- get.source(source)

  if(is.null(my_geog)) my_geog <- my.geog()
  if(is.null(other_geogs)) other_geogs <- my.other.geogs()

  if(my_geog=="") my_geog <- character(0)

  geogs <- get.geogs()

  ver<-integer(0)
  if(main) ver<-0

  vermax <- highest_version(year)
  if(vermax == 0) versions = FALSE

  if(versions) ver<-c(ver,1:vermax)

  df_geogs <- get.geogs.all()

  sapply(ver,function(version) {


    if(source == 'sas') {
      rdata_file <- apply.pattern("sas_data_path",YEAR = year, VERS = version)
    } else {
      rdata_file <- apply.pattern("ascii_path", YEAR = year, VERS = version)
    }

    df_brfss <- orrr::get.rdata(file = rdata_file)

    if(geogs[1] == '*') {
      geogs<-unique(df_brfss$`_STATE`)

    } else {

      if(is.character(geogs)) {
        geogs<-sapply(geogs,function(state) {
          df_geogs[df_geogs$Abbrev==state,"Id"]
        })

        geogs <- unlist(unname(geogs))
      }
    }

    add_cols<-character(0)

    # fldr_geog <- normalizePath(apply.pattern("brfss_geog_folder", YEAR = year, GEOG= geog),
    #                            winslash = "/", mustWork = FALSE)


    mapply(function(id,nm) {

      if(id%in%geogs) {

        df_state<-df_brfss[df_brfss$`_STATE`==id,]

        if(nrow(df_state)>0) {
          if(verbose) cat("Saving ",nm,"V",version,"\n")
          sapply(1:ncol(df_brfss),function(i) {
            attrs<-attributes(df_brfss[[i]])

            if(!is.null(attrs)){
              sapply(1:length(attrs),function(j) {
                #browser()
                attr(df_state[[i]],names(attrs[j]))<<-attr(df_brfss[[i]],names(attrs[j]))
              })
            } else {
              add_cols<<-c(add_cols,colnames(df_brfss)[i])
            }

          })

          dfname<-paste0("df_",nm,"_",year)
          if(version>0) dfname<-gsub(nm,paste0(nm,"_V",version),dfname)

          assign(dfname,df_state)

          fname <- brfss_data_path(year = year, geog = nm, version = version, rw = 'w')

          if(verbose) cat("Going to save :", fname, "\n")

          save(list = c(dfname),file = fname)

          #columns.add(year,add_cols)
        }
      }
    },df_geogs$Id,df_geogs$Abbrev)
  })
  invisible()
}


#' Process BRFSS Annual Data Files
#' Use this function to process a single year of BRFSS data. This function sill download, unzip,
#' and create a data frame with all data from the XPT file, as well as any other versions of the survey.
#'
#' @param year - int - year of interest
#' @param source character - data source ('sas' or 'ascii')
#' @param download - logical - download the data? Useful (set = FALSE) if you already have the downloaded files
#' @param convert - logical - read the downloaded files into data_frames and save? Useful (set = FALSE) if you already have the downloaded files processed
#' @param codebook - logical - download and process the annual codebook
#' @param split - logical - split the processed data file by state/geography
#' @param ... other params
#'
#' @return invisible()
#' @export
#'

process_year <- function(year = NULL, source = NULL, download=TRUE, convert=TRUE, codebook = TRUE,
                         split = TRUE, verbose=FALSE, ...) {

  source <- get.source(source)
  source<-match.arg(source,c("sas","ascii"))

  if(source == 'sas') {
    sas_process_year(year = year, download=download, layout = TRUE, convert=convert, codebook = codebook,
                     split = split, verbose=verbose, ...)
  } else {
    ascii_process_year(year = year, download=download, convert=convert, codebook = codebook,
                       split = split, verbose=verbose, ...)
  }

}
