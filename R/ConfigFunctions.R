CreateConfig <- function(databasePath,
                         configPath = "~/.ScopusConfig.yml"){

  databasePath <- paste("  database: '", databasePath, "'",
                        sep = "")

  writeLines(c("default:", databasePath), configPath)
}

GetConfig <- function(value,
                      config = "default",
                      file = "~/.ScopusConfig.yml"){
  if(file.exists(file) == TRUE){
    config::get(value = value,
                config = config,
                file = file)
  } else {
    message("The configuration file does not exist. Create it first with CreateConfig()")
    stop()
  }
}
