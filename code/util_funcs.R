

brfss.data <- function(year = NULL, geog = NULL, version = 0) {

  year <- get.year(year)
  geog <- get.geog(geog)

  path <- apply.pattern("brfss_state_path",YEAR = year, GEOG = geog, VERS = version)

}
