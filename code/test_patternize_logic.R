




patternize<-function(strIn, ...) {

  args <- list(...)

  ##  remove args with NULL value

  args <- args[!sapply(args,is.null)]

  nms <- names(args)
  vals <- unlist(unname(args))

  ###########################################################################
  ##
  ##  this is a special case for YEAR ... creating the 2-digit year as well
  yr_arg <- which(nms == "YEAR")

  if(length(yr_arg)>0) {
    year <- as.integer(vals[yr_arg[1]])
    nms[length(nms)+1] <- "YR"
    vals[length(vals)+1] <- year%%100

  }

  #############################################################################
  ret <- strIn

  mapply(function(nm,val) {
    nm <- paste0("[",nm,"]")
    ret<<-gsub(nm,val,ret,fixed = T)


  }, nms, vals)

  #######################################################################
  ##
  ##    check for logical expressions
  ##    e.g. "LLCP([VERS]==0;[YEAR])([VERS]>0;[YR]V[VERS])_XPT.zip"

  #check <- grepl(".*[(](.*)[)].*",ret)

  while(grepl(".*[(](.*)[)].*",ret)) {

    expr <- gsub(".*[(](.*)[)].*","\\1",ret)

    expr <-  gsub("(.*);(.*)","\\1",expr)
    ok <- eval(parse(text = expr))

    if (ok) {
      ret <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\2\\3",ret)
    } else {
      ret <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\3",ret)
    }

    check <- grepl(".*[(](.*)[)].*",ret)

  }

  ret
}


name <- "test_version"
pattern <- "LLCP([VERS]==0;[YEAR])([VERS]>0;[YR]V[VERS])_XPT.zip"

set.pattern(name = name,pattern = pattern)

x <-patternize(pattern, YEAR = 2020, VERS = 1)


check <- grepl(".*[(](.*)[)].*",x)

#gsub(".*[(](.*)[)].*","\\1",x)

check

while(check) {
  expr <- gsub(".*[(](.*)[)].*","\\1",x)

  expr <-  gsub("(.*);(.*)","\\1",expr)
  ok <- eval(parse(text = expr))

  if (ok) {
    x <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\2\\3",x)
  } else {
    x <- gsub("(.*)[(].*;(.*)[)](.*)","\\1\\3",x)
  }

  check <- grepl(".*[(](.*)[)].*",x)

}

x


