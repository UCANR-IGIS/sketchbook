#' Get cleaned CIMIS data from PGFarm using the public account
#' This function is derived from cch_clncimis_qry from the cherrychill package (which is currently private).
#' For details and bug reports, please contact Andy Lyons

cleancimis_qry <- function(station_n = NULL, start_dt = NULL, end_dt = NULL, quiet = FALSE) {

  library(DBI)
  library(RPostgres)
  library(glue)
  library(cli)
  library(dplyr)
  library(lubridate)
  
  if (is.null(start_dt) != is.null(end_dt)) {
    cli::cli_abort("You must provide either both {.var start_dt} and {.var end_dt}, or neither")  
  }
  
  if (is.null(start_dt) && is.null(end_dt)) {
    where_dt <- ""
    tz_str <- "America/Los_Angeles"
    
  } else {
    if (!is.null(start_dt)) {
      if (!inherits(start_dt, "POSIXct")) cli::cli_abort("{.var start_dt} must be class POSIXct")  
    }
    if (!is.null(end_dt)) {
      if (!inherits(end_dt, "POSIXct")) cli::cli_abort("{.var start_dt} must be class POSIXct")
    }
    ## Get the time zone
    if (tz(start_dt) != tz(end_dt)) cli::cli_abort("{.var start_dt} and {.var end_dt} must be in the same timezone")
    tz_str <- tz(start_dt)
    
    ## Construct the where clause
    start_chr <- format(start_dt, paste0("%Y-%m-%d %H:%M:%S ", tz_str))
    end_chr <- format(end_dt, paste0("%Y-%m-%d %H:%M:%S ", tz_str))
    where_dt <- glue("date_time BETWEEN '{start_chr}' AND '{end_chr}'")
    
  }
  
  ## Construct the WHERE clause for the stations
  if (is.null(station_n)) {
    and_keyword <- ""
    where_stn <- ""
    
  } else {
    if (where_dt == "") {
      and_keyword <- ""
    } else {
      and_keyword <- "AND" 
    }
    
    if (!is.numeric(station_n)) cli::cli_abort("{.var station_n} must be a numeric vector")
    stations_chr <- paste(station_n, collapse = ", ")
    where_stn <- glue("station_n IN ({stations_chr})")
    
  }
  
  if (where_dt == "" && where_stn == "") {
    where_keyword <- ""
  } else {
    ## There is at least one where clause
    where_keyword <- "WHERE"
    
  }
  
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

  cimis_tbl <- dbGetQuery(con,
                          glue("SELECT * FROM api.cimis_clean {where_keyword} {where_dt} {and_keyword} {where_stn};")) |> 
    as_tibble() |>
    mutate(date_time = with_tz(date_time, tz_str)) |>
    arrange(date_time, station_n)

  if (nrow(cimis_tbl) == 0) {
    if (!quiet) cli_alert_warning("Sorry, no rows were returned.")
    return(NULL)
  } else {
    if (!quiet) {
      cli_alert_info("Retrieved {nrow(cimis_tbl)} rows ")
    }
  }

  dbDisconnect(con)

  cimis_tbl

}
