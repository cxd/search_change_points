
# Load configuration per environment.
loadConfig <- function() {
  host <- system("hostname", intern=TRUE)
  
  if(file.exists(paste(getwd(), "env", host, "config.R", sep="/"))){
    cfgDir <- host
  } else {
    cfgDir <- "default"
  }
  #Source the configuration, utility, data processing, and report generation files.
  source(paste(getwd(), "env", cfgDir, "config.R", sep="/"))
  getConfig()
}