#' Get the stations in the cleaned CIMIS data from PGFarm using the public account
#' For details and bug reports, please contact Andy Lyons

cleancimis_stn <- function(quiet = FALSE) {

  library(DBI)
  library(RPostgres)
  library(cli)
  library(dplyr)
  
  ## Connect using the public account
  con <- dbConnect(
    RPostgres::Postgres(),
    user = 'pgfarm-public',
    host = 'pgfarm.library.ucdavis.edu',
    port = 5432,
    dbname = 'uc-anr/tree-chill',
    password = 'go-aggies'
  )

  
  if (!quiet) {
    cli::cli_li("Querying CIMIS data on PG Farm")
    cli::cli_alert_info("This can take up to 30 seconds if the database has been asleep.")
  }

  stn_tbl <- dbGetQuery(con, "SELECT DISTINCT station_n FROM api.cimis_clean;") |> 
    as_tibble() |> 
    arrange(station_n)
  
  if (nrow(stn_tbl) == 0) {
    if (!quiet) cli_alert_warning("Sorry, no rows were returned.")
    return(NULL)
  } else {
    if (!quiet) {
      cli_alert_info("Retrieved {nrow(stn_tbl)} unique station ids")
    }
  }

  dbDisconnect(con)

  stn_tbl

}
