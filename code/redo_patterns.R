library(dplyr)

  file1 = "./R/pattern_init.R"
  file2 = "./R/patterns.R"

  source(file1)
  source(file2)


  init.patterns()

  rm(list = ls())

 # devtools::build()
