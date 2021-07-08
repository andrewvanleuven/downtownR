library(tidyverse); library(SpatialKDE); library(sf); library(viridis); library(tidycensus); 
library(tigris); library(janitor); library(ggpubr); library(data.table)

# Read in data ------------------------------------------------------------
source('code/functions.R')
df <- info_clean('hidden/ny_2005.csv') %>% 
  filter(match_code %in% c('0','P'), !is.na(latitude) & !is.na(longitude) & !is.na(jobs) & !is.na(naics)) %>% 
  st_as_sf(., coords = c('longitude','latitude'), crs = 4326, remove = F) %>%
  st_transform(.,crs = 3649)
df2 <- info_clean('hidden/pa_2005.csv') %>% 
  filter(match_code %in% c('0','P'), !is.na(latitude) & !is.na(longitude) & !is.na(jobs) & !is.na(naics)) %>% 
  st_as_sf(., coords = c('longitude','latitude'), crs = 4326, remove = F) %>%
  st_transform(.,crs = 3649)
df_nypa <- bind_rows(df,df2) %>% 
  mutate(naics3 = str_sub(naics,end = 3))
univ <- fread('data/univ_nypa.csv')
# beepr::beep(2)

# GeoJoin Biz and Univ Data -----------------------------------------------
city <- places(c('36','42'),T) %>%
  st_transform(.,crs = 3649) %>%
  mutate(city_fips = as.numeric(GEOID)) %>%
  select(city_fips,city_name = NAME,geometry) %>%
  filter(city_fips %in% (univ %>% pull(city_fips)))
# crsuggest::suggest_crs(city)
df_geo <- st_intersection(city,df_nypa)
beepr::beep(2)
fwrite(df_geo,'hidden/ny_pa_2005.csv')
