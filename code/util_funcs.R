
geog.name <- function(geog) {

  df_states <- orrr::get.rdata("./data/states.RData")

  if(is.character(geog) && nchar(geog)==2) {
    geog <- df_states %>%
      filter(Abbrev == {{geog}}) %>%
      pull(State)

  } else if(is.numeric(geog)) {
    geog <- df_states %>%
      filter(Id == {{geog}}) %>%
      pull(State)

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

brfss.data <- function(year, geog, version = 0) {

  path <- apply.pattern("brfss_state_path",year = year, abb = geog,version = version)

}
