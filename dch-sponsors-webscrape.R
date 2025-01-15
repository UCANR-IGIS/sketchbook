library(rvest)
library(xml2)
library(dplyr)
library(lubridate)
library(stringr)

all_counties_tbl <- NULL

for (i in 1:58) {
  
  one_county_url <- paste0("https://cacfp.dss.ca.gov/DayCareHomeSponsors/PartialCounty?countyId=", i)
  
  cat(" - Processing .../", basename(one_county_url), ". ", sep = "")
  
  ## Pull out the xml for the page
  one_county_xml <- read_html(one_county_url)
  
  ## Get the county name
  county_name_chr <- one_county_xml |> 
    html_elements("title") |> 
    xml_text() |> 
    str_extract(pattern = "(?<=DCH Information For ).*(?= County)")
  
  ## View the HTML Tables in this document
  one_county_htmltbls_lst <- one_county_xml |> 
    html_elements("table")
  
  ## Check to see if no tables were found
  if (length(one_county_htmltbls_lst) != 1) stop(paste0("Didn't find one and only one table in HTML page \n",
                                                        one_county_url))
  
  ## Convert the first (and only) HTML table into a tibble
  first_table_tbl <- one_county_htmltbls_lst[[1]] |> html_table()
  
  ## Make sure it had at least one row
  if (nrow(first_table_tbl) == 0) {
    stop(paste0("Empty table found in ", baseName(one_county_url)))
    
  ## Check for 'no sponors' 
  } else if (str_detect(first_table_tbl[1,1, drop = TRUE], pattern = "^There are currently no CACFP DCH sponsors operating")) {
    cat(paste0(" - No providers in ", county_name_chr, "\n"))
    
  ## Clean it up and append
  } else {
    one_county_tbl <- first_table_tbl |>   
      rename(sponsor_name = `Sponsor Name`,
             street = Street,
             city = City,
             state = State,
             zip = Zip,
             phone = Phone,
             partial_cov = `Partial County Coverage`,
             lang_othr = `Other Languages Spoken`,
             last_updated = `Last Updated`) |> 
      mutate(last_updated = mdy(last_updated),
             zip = as.character(zip)) |>
      mutate(county = county_name_chr, .before = "sponsor_name")
    
    ## Feedback
    cat("County name: ", county_name_chr, ". Num recs = ", nrow(one_county_tbl), ".\n", sep = "")
    
    ## Append
    all_counties_tbl <- all_counties_tbl |> bind_rows(one_county_tbl)
    
  }

}

all_counties_tbl

## Next Steps

## To parse the partial_coverage_county and/or lang_other columns, you can pivot longer or wider
## See https://ucanr-igis.github.io/sketchbook/delimited-column-pivot-longer.nb.html

## Save it as a CSV

