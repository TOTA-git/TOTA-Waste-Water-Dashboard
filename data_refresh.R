# Data refresh job.
#
# Fetches every externally-sourced dataset the app depends on and writes
# each one to data/cache/. Each dataset is fetched and written
# independently: if one source fails (site down, API change, network
# blip), that dataset's existing cache file is left untouched and the
# rest of the refresh continues. global.R only ever reads from
# data/cache/, so the app always has the most recent successful pull
# of each dataset, even if today's refresh partially failed.
#
# Run manually with:
#   Rscript data_refresh.R
#
# To run this once a day.
# (e.g.GitHub Actions)

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(geojsonsf)
library(rvest)
library(chromote)
library(stringr)

source("R/data_helpers.R")

clear_refresh_log()

refresh_dataset <- function(name, fetch, write) {
  message("Refreshing ", name, "...")
  tryCatch(
    {
      value <- fetch()
      write(value)
      log_refresh(name, "success", n_rows = NROW(value))
      message("  OK (", NROW(value), " rows)")
      TRUE
    },
    error = function(e) {
      log_refresh(name, "failure", message = conditionMessage(e))
      message("  FAILED - keeping previous cache: ", conditionMessage(e))
      FALSE
    }
  )
}

#STATS CAN WASTEWATER VOLUMES PROCESSED BY MUNICIPAL SEWAGE SYSEMS
refresh_dataset(
  "basin_volumes",
  function() {
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE), add = TRUE)
    
    download.file(
      "https://www150.statcan.gc.ca/n1/tbl/csv/38100099-eng.zip",
      destfile = tmp_zip,
      quiet = TRUE,
      mode = "wb"
    )
    unzip(zipfile = tmp_zip, exdir = tmp_dir)
   data_unfiltered <- read.csv(file.path(tmp_dir, "38100099.csv"))
   data <- data_unfiltered %>%
     filter(GEO == "Okanagan-Similkameen drainage region" | GEO == "Fraser-Lower Mainland drainage region" | GEO == "Columbia drainage region" |
              GEO == "Pacific Coastal drainage region" | GEO == "Peace-Athabasca drainage region" | GEO == "Lower Mackenzie drainage region" |
              GEO == "Yukon drainage region") %>% 
     mutate(
       DR_Name = case_when(
         GEO == "Fraser-Lower Mainland drainage region" ~ "Fraser Lower Mainland",
         GEO == "Columbia drainage region" ~ "Columbia",
         GEO == "Okanagan-Similkameen drainage region" ~ "Okanagan Similkameen",
         GEO == "Pacific Coastal drainage region" ~ "Pacific Coast",
         GEO == "Peace-Athabasca drainage region" ~ "Peace Athabasca",
         GEO == "Lower Mackenzie drainage region" ~ "Lower Mackenzie",
         GEO == "Yukon drainage region" ~ "Yukon",
         TRUE ~ NA_character_
       )
     ) %>%
     select(REF_DATE, DR_Name, Month, UOM, SCALAR_FACTOR, VALUE, STATUS)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "basin_volumes.csv"))
)

#STATS CAN WASTEWATER VOLUME RELEASE DATE - apparently stats can will no longer be updating this.
refresh_dataset(
  "stats_can_release_date",
  function() {
    webpage <- read_html("https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=3810009901")
    
    release <- html_node(webpage, '.viewSubtitle:nth-child(3)')
    
    date_text = html_text(release)
    just_date <- as.Date(str_extract(date_text, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
    df <- as.data.frame(just_date)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "stats_can_release_date.csv"))
)

#STATS CAN POPULATION SERVED BY MUNICIPAL SEWAGE SYSEMS
refresh_dataset(
  "population_served",
  function() {
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(c(tmp_zip, tmp_dir), recursive = TRUE), add = TRUE)
    
    download.file(
      "https://www150.statcan.gc.ca/n1/tbl/csv/38100119-eng.zip",
      destfile = tmp_zip,
      quiet = TRUE,
      mode = "wb"
    )
    unzip(zipfile = tmp_zip, exdir = tmp_dir)
    data_unfiltered <- read.csv(file.path(tmp_dir, "38100119.csv"))
    data <- data_unfiltered %>%
      filter(GEO == "Okanagan-Similkameen drainage region" | GEO == "Fraser-Lower Mainland drainage region" | GEO == "Columbia drainage region" |
               GEO == "Pacific Coastal drainage region" | GEO == "Peace-Athabasca drainage region" | GEO == "Lower Mackenzie drainage region" |
               GEO == "Yukon drainage region") %>% 
      mutate(
        GEO = case_when(
          GEO == "Fraser-Lower Mainland drainage region" ~ "Fraser Lower Mainland",
          GEO == "Columbia drainage region" ~ "Columbia",
          GEO == "Okanagan-Similkameen drainage region" ~ "Okanagan Similkameen",
          GEO == "Pacific Coastal drainage region" ~ "Pacific Coast",
          GEO == "Peace-Athabasca drainage region" ~ "Peace Athabasca",
          GEO == "Lower Mackenzie drainage region" ~ "Lower Mackenzie",
          GEO == "Yukon drainage region" ~ "Yukon",
          TRUE ~ NA_character_
        )
      ) %>%
      select(REF_DATE, GEO, VALUE, STATUS)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "population_served.csv"))
)

#STATS CAN POPULATION SERVED RELEASE DATE - apparently stats can will no longer be updating this.
refresh_dataset(
  "population_release_date",
  function() {
    webpage <- read_html("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3810011901")
    
    release <- html_node(webpage, '.viewSubtitle:nth-child(3)')
    
    date_text = html_text(release)
    just_date <- as.Date(str_extract(date_text, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
    df <- as.data.frame(just_date)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "population_release_date.csv"))
)

#EFFLUENT IDENTIFICATION ---------------------------------------------------------------------------------------------
refresh_dataset(
  "effluent_id",
  function() {
    url <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fplaninfrastruture%2Fwastewater-systems-effluent-regulations-reported-data%2FResaeu-Wser-identification.csv"

    df <- read.csv(
      url,
      fileEncoding = "Windows-1252",
      encoding = "UTF-8",
      check.names = FALSE
    )
    
    df <- df %>%
      rename(id = `Identification de l'installation/ Facility Identification`)
    
    omrr_sf <- st_as_sf(
      df,
      coords = c("Longitude/ Longitude", "Latitude/ Latitude"),
      crs = 4326,
      remove = FALSE
    )
    
    region <- st_read("data/thompson_okanagan_boundary.geojson", quiet = TRUE)
    
    region <- st_transform(region, st_crs(omrr_sf))
    
    # Keep only OMRR locations inside the thompson okanagna tourism region
    omrr_sf <- st_filter(
      omrr_sf,
      region,
      .predicate = st_within
    )
    
    # Remove geometry before saving CSV
    st_drop_geometry(omrr_sf)
    
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "effluent_id.csv"))
)

#EFFLUENT MONITORING ---------------------------------------------------------------------------------------------
refresh_dataset(
  "effluent_monitoring",
  function() {
    url <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fplaninfrastruture%2Fwastewater-systems-effluent-regulations-reported-data%2FResaeu-Wser-surveillance-monitoring.csv"
    
   df <- readr::read_csv(
        url,
        locale = readr::locale(encoding = "Windows-1252"),
        show_col_types = FALSE
      )
  df <- df %>%
    select(`Identification de l'installation/ Facility Identification`, `Période de déclaration/ Reporting Period`, `Nom du propriétaire/ Owner Name`, `Nom du système/ System Name`,
           `Ville / City`, `Mois/ Month`, `Volume journalier moyen de l'effluent (m3)/ Average Daily Effluent Volume (m3)`, `Périodes decalcul (Anglais)/ Averaging Period (English)`,
           `Volume total de l’effluent rejeté en mètre cube (m3) pour la période de calcul de la moyenne/ Total volume of effluent deposited in cubic metres (m3) for averaging period`,
           `Nombre total de jours où l’effluent a été rejeté durant la période de calcul de la moyenne/ Total number of days effluent was deposited for averaging period`,
           `Moyenne des matières en suspension (mg/L) pour la période de calcul de la moyenne/ Average suspended solids (mg/L) for averaging period`, `Limite des matières en suspension/ Suspended Solids Limit`,
    ) %>% 
    rename(
      id = `Identification de l'installation/ Facility Identification`,
      reporting_period = `Période de déclaration/ Reporting Period`,
      owner_name = `Nom du propriétaire/ Owner Name`,
      system_name = `Nom du système/ System Name`,
      city = `Ville / City`, 
      month = `Mois/ Month`,
      avg_daily_eff_m3 = `Volume journalier moyen de l'effluent (m3)/ Average Daily Effluent Volume (m3)`,
      period = `Périodes decalcul (Anglais)/ Averaging Period (English)`,
      eff_deposited_m3 = `Volume total de l’effluent rejeté en mètre cube (m3) pour la période de calcul de la moyenne/ Total volume of effluent deposited in cubic metres (m3) for averaging period`,
      days_desposited = `Nombre total de jours où l’effluent a été rejeté durant la période de calcul de la moyenne/ Total number of days effluent was deposited for averaging period`,
      avg_ss_mgL = `Moyenne des matières en suspension (mg/L) pour la période de calcul de la moyenne/ Average suspended solids (mg/L) for averaging period`,
      ss_limit = `Limite des matières en suspension/ Suspended Solids Limit`
    ) 
  
  df
  
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "effluent_monitoring.csv"))
)


#EFFLUENT LAST MODIFIED ----------------------------------------------------------------------------------------------
refresh_dataset(
  "effluent_lastModified",
  function(){
    data <- fromJSON("https://open.canada.ca/data/api/action/package_show?id=9e11e114-ef0d-4814-8d93-24af23716489")
    
      as.data.frame(data$result$resources$date_published[1])
      
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "effluent_lastModified.csv"))
)

message("Data refresh complete. See ", file.path(CACHE_DIR, "refresh_log.csv"), " for a run history.")