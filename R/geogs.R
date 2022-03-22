

save.geogs <- function(geogs) {
  file <-paste0(get.pattern("data_folder"),"geogs.rda")
  save(geogs, file = file)
}

get.geogs <- function() {

  data("geogs", package="brfss")
  geogs
}


geog.name <- function(geog) {

  geogs <- get.geogs()

  if(is.character(geog) && nchar(geog)==2) {
    geog <- geogs %>%
      filter(Abbrev == {{geog}}) %>%
      pull(Geog)

  } else if(is.numeric(geog)) {
    geog <- geogs %>%
      filter(Id == {{geog}}) %>%
      pull(Geog)

  }

  return (geog)
}

geog.abb <- function(geog) {

  geogs <- get.geogs()

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- geogs %>%
      filter(Geog == {{geog}}) %>%
      pull(Abbrev)

  } else if(is.numeric(geog)) {
    geog <- geogs %>%
      filter(Id == {{geog}}) %>%
      pull(Abbrev)

  }

  return (geog)
}

geog.id <- function(geog) {

  geogs <- get.geogs()

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- geogs %>%
      filter(Geog == {{geog}}) %>%
      pull(Id)

  } else if(is.character(geog)) {
    geog <- geogs %>%
      filter(Abbrev == {{geog}}) %>%
      pull(Id)

  }

  return (geog)
}
