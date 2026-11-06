#' Get cleaned CIMIS data from PGFarm using the public account
#' This function is derived from cch_clncimis_qry from the cherrychill package (which is currently private).
#' For details and bug reports, please contact Andy Lyons

cleancimis_qry <- function(station_n = NULL, start_dt, end_dt, quiet = FALSE) {

  library(DBI)
  library(RPostgres)
  library(glue)
  library(cli)
  library(dplyr)
  library(lubridate)

  if (!inherits(start_dt, "POSIXct")) cli::cli_abort("{.var start_dt} must be class POSIXct")
  if (!inherits(end_dt, "POSIXct")) cli::cli_abort("{.var start_dt} must be class POSIXct")
  if (tz(start_dt) != tz(end_dt)) cli::cli_abort("{.var start_dt} and {.var end_dt} must be in the same timezone")

  ## Get the time zone
  tz <- tz(start_dt)

  ## Connect using the public account
  con <- dbConnect(
    RPostgres::Postgres(),
    user = 'pgfarm-public',
    host = 'pgfarm.library.ucdavis.edu',
    port = 5432,
    dbname = 'uc-anr/tree-chill',
    password = 'go-aggies'
  )

  start_chr <- start_dt |>
    format(paste0("%Y-%m-%d %H:%M:%S ", tz))

  end_chr <- end_dt |>
    format(paste0("%Y-%m-%d %H:%M:%S ", tz))

  ## Construct the WHERE clause for the stations
  if (is.null(station_n)) {
    where_stn <- ""
  } else {
    if (!is.numeric(station_n)) cli::cli_abort("{.var station_n} must be a numeric vector")
    stations_chr <- paste(station_n, collapse = ", ")
    where_stn <- glue("AND station_n IN ({stations_chr})")
  }

  if (!quiet) {
    cli::cli_li("Querying CIMIS data on PG Farm")
    cli::cli_alert_info("This can take up to 30 seconds if the database has been asleep.")
  }

  cimis_tbl <- dbGetQuery(con,
                          glue("SELECT * FROM api.cimis_clean
                               WHERE date_time BETWEEN '{start_chr}' AND '{end_chr}'
                               {where_stn};")) |>
    as_tibble() |>
    mutate(date_time = with_tz(date_time, tz)) |>
    arrange(date_time, station_n)

  if (nrow(cimis_tbl) == 0) {
    if (!quiet) cli_alert_warning("Sorry, no rows were returned.")
    return(NULL)
  } else {
    if (!quiet) {
      time_range_chr <- cimis_tbl$date_time |>
        range() |>
        format("%Y-%m-%d %H:%M")
      cli_alert_info("Retrieved {nrow(cimis_tbl)} rows from {.field {time_range_chr[1]}} to {.field {time_range_chr[2]}}")
    }
  }

  dbDisconnect(con)

  cimis_tbl

}
