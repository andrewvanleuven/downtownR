library(sf)
library(data.table)
library(tidyverse)

# Read in data ------------------------------------------------------------
df <- fread("/Users/andrew/Documents/GitHub/smallbiz/hidden/infogroup/ny05geo.csv") %>%
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>%
  st_transform(.,crs = 3174)
ny_univ <- fread("data-raw/ny_univ.csv")

ny <- df %>%
  filter(match_code != "X",
         cty_fips == 36013) %>%
  st_drop_geometry() %>%
  left_join(ny_univ %>% select(city_fips,rucc,cz,pop_2010), by = "city_fips") %>%
  arrange(city_fips) %>%
  mutate(id = str_pad(rownames(.),width = 6,pad = "0")) %>%
  select(id,city_fips,city_name,jobs,naics3,cty_fips,rucc,cz,pop_2010,latitude,longitude)

beepr::beep()

usethis::use_data(ny,overwrite = T)
