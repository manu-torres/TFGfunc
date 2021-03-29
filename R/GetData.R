GetQuery <- function(query,
                     databasePath = NA){
  if(is.na(databasePath) == TRUE){
    database <- GetConfig("database")
  } else if(is.character(databasePath) == TRUE){
    database <- databasePath
  } else {
    message("No valid database path has been specified and there is no config file to read a default.")
    stop()
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), database)

  QueryResults <- DBI::dbGetQuery(conn, query)

  DBI::dbDisconnect(conn)

  return(QueryResults)
}
