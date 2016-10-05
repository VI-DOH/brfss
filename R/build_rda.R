

build_monthlies<-function(year) {
  env<-new.env()
  sapply(1:12,function(mnth) {
    fpath<-brfss_raw_filename(type = "cell",year=year,month=mnth)
    if(file.exists(fpath)) {
      df<-read.brfss.cell(year,month = mnth,index = 1,completes = "vi")
    }
  })

}
