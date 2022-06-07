

#' Print Results to pdf File
#'
#' @return nothing
#' @export
#'
#'
#'
brfss.results.pdf<-function() {
  #file_in<-system.file("output", "questionnaire_results.Rmd", package = "brfssvi")

  file<-"./output/monthly_reports/questionnaire_results"
  file_out<-paste(file,"pdf",sep=".")


  rmarkdown::render(input = file_in)

}

