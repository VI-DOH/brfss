
save.geogs <- function(geogs) {
  file <-paste0(get.pattern("data_folder"),"geogs.rda")
  save(geogs, file = file)
}

get.geogs <- function() {

  geogs <- orrr::get.rdata("./data/geogs.rda")
  if(is.null(geogs)) {

    data("geogs")
    save.geogs(geogs)
  }

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

  df_states <- orrr::get.rdata("./data/states.RData")

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- df_states %>%
      filter(State == {{geog}}) %>%
      pull(Abbrev)

  } else if(is.numeric(geog)) {
    geog <- df_states %>%
      filter(Id == {{geog}}) %>%
      pull(Abbrev)

  }

  return (geog)
}

geog.id <- function(geog) {

  df_states <- orrr::get.rdata("./data/states.RData")

  if(is.character(geog) && nchar(geog)!=2) {
    geog <- df_states %>%
      filter(State == {{geog}}) %>%
      pull(Id)

  } else if(is.character(geog)) {
    geog <- df_states %>%
      filter(Abbrev == {{geog}}) %>%
      pull(Id)

  }

  return (geog)
}
