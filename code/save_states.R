
states<-read.csv(file = "./data-raw/states.csv",stringsAsFactors = F)


devtools::use_data(states,internal = T)


rm(states)
