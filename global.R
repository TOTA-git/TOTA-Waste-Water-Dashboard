library(ggplot2)
library(readr)
library(dplyr)
library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)
library(bcmaps)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(tidyr)

source("R/data_helpers.R")

# externally-sourced data below is
# populated by data_refresh.R on a schedule. The app itself never
# scrapes or calls an external API directly at startup - if a scheduled
# refresh fails for a dataset, its cache file is left untouched, so the
# app keeps running on the last successful pull (see
# data/cache/refresh_log.csv for a history of refresh attempts).

#WASTEWATER VOLUMES PROCESSED BY MUNICIPAL SEWAGE SYSTEMS ============================================================
#VOLUMES B.C. VALUE to be x1000000 
df_basin_vol_all <- read_cache_csv("basin_volumes")

#VOLUMES THOMPSON OKANAGAN FILTERED - VALUE to be x1000000 
df_basin_vol_TO <- df_basin_vol_all %>%
  filter(DR_Name == "Okanagan Similkameen" | DR_Name == "Fraser Lower Mainland" | DR_Name == "Columbia")

#FRESHNESS OF DRAINAGE BASIN VOLUMNES DATA - Updated annually by Stats Canada
df_stats_freshness <- read_cache_csv("stats_can_release_date")

#POPULATION SERVED BY MUNICIPAL WASTEWATER SYSTEMS ===================================================================
#POPULATION SERVED FOR BC
df_pop_BC <- read_cache_csv("population_served")

#POPULATIONS SERVED FOR BASINS THAT MAKE UP T-O
df_pop_TO <- df_pop_BC %>%
  filter(GEO == "Okanagan Similkameen" | GEO == "Fraser Lower Mainland" | GEO == "Columbia") 

all_regions <- df_pop_TO %>%
  group_by(REF_DATE) %>%
  summarise(
    GEO = "All Regions",
    VALUE = sum(VALUE, na.rm = TRUE),
    .groups = "drop",
    STATUS = "A"
  )

df_pop_TO <- bind_rows(df_pop_TO, all_regions) %>%
  arrange(REF_DATE, desc(GEO == "All Regions"))

#FRESHNESS Updated annually by Stats Canada
df_pop_freshness <- read_cache_csv("population_release_date")

#WASTEWATER PROCESSED PER PERSON SERVED ==============================================================================
df_temp <- df_basin_vol_TO %>% 
  filter(Month == "Total volume, all months") %>%
  rename(
    GEO = DR_Name,
    Volume = VALUE
  ) 
df_temp_all_regions <- df_temp %>% #add all regions row for Volume data
  group_by(REF_DATE) %>%
  summarise(
    GEO = "All Regions",
    Volume = sum(Volume, na.rm = TRUE),
    .groups = "drop"
  )

df_temp <- bind_rows(df_temp, df_temp_all_regions) %>%
  arrange(REF_DATE, desc(GEO == "All Regions"))

df_pop_vol_TO <- df_temp %>% #join vol and pop data. Compute municipal wastewater processed per person served per year
  left_join(
    df_pop_TO %>%
      rename(Population = VALUE),
    by = c("REF_DATE", "GEO")
  ) %>%
  mutate(
    vol_person_year = (Volume * 1000000) / Population
  )

df_pop_vol_TO <- df_pop_vol_TO %>%
  select(REF_DATE, GEO, Volume, Population, vol_person_year)

#EFFLUENT ============================================================================================================
#EFFLUENT ID
df_effluent_id <- read_cache_csv("effluent_id")

#EFFLUENT MONITORING
df_effluent_monitoring <- read_cache_csv("effluent_monitoring")


#FINAL EFFLUENT DF
df_effluent <- df_effluent_monitoring %>%
  mutate(
    year = format(as.Date(str_sub(reporting_period,1,4), "%Y"), "%Y")
  ) %>%
  semi_join(
    df_effluent_id,
    by = "id"
  )

#EFFLUENT_FRESHNESS
df_effluent_freshness <- read_cache_csv("effluent_lastModified")

#BOUNDARIES ==========================================================================================================
#DRAINAGE BASIN BOUNDARIES
df_basin_bounds <- st_read("data/drainage_basin_boundaries.geojson", quiet = TRUE)

#BC BOUNDARY
df_bc <- st_read("data/bc_boundary.geojson", quiet = TRUE)

#THOMPSON OKANAGAN BOUNDARY
df_TO_boundary <- st_read("data/thompson_okanagan_boundary.geojson", quiet = TRUE)

