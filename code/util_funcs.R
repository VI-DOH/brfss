

brfss.data <- function(year, geog, version = 0) {

  path <- apply.pattern("brfss_state_path",YEAR = year, GEOG = geog, VERS = version)

}
