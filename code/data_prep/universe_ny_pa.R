library(tidyverse); library(tidycensus); library(sf); library(tigris)
source('code/functions.R')

# Set up state universes --------------------------------------------------
rucc <- read_csv("data/rucc.csv") %>% select(cty_fips,rucc)
cz <- read_csv("data/cz.csv") %>% rename(cz = cz2000) %>% select(cty_fips,cz)
hist_pops <- read_csv("data/hist_pops_NY.csv") %>% 
  bind_rows(read_csv("data/hist_pops_PA.csv")) %>% 
  select(title, pop_1910 = y1910, pop_1920 = y1920, pop_1930 = y1930, pop_1940 = y1940)
ny_cities <- get_decennial(geography = "place",
                           variables = 'P001001') %>%
  mutate(st_fips = as.numeric(str_sub(GEOID,0,2))) %>%
  filter(st_fips == 36) %>%
  separate(NAME,c("name","st"),sep = ",") %>%
  rename_all(tolower) %>%
  trim_census() %>%
  mutate(city_fips = as.numeric(geoid)) %>%
  select(city_fips, name, st, pop_2010 = value) %>%
  arrange(city_fips)
ny_ctys <- counties("NY",T,"20m") %>%
  mutate(cty_fips = as.numeric(GEOID)) %>%
  st_transform(.,crs = 3174) %>%
  select(cty_fips, geometry)
ny_centroid <- places("NY",T) %>%
  st_transform(.,crs = 3174) %>%
  st_centroid() %>%
  mutate(city_fips = as.numeric(GEOID)) %>%
  select(city_fips, geometry) %>%
  st_intersection(ny_ctys) %>%
  st_drop_geometry()
pa_cities <- get_decennial(geography = "place",
                           variables = 'P001001') %>%
  mutate(st_fips = as.numeric(str_sub(GEOID,0,2))) %>%
  filter(st_fips == 42) %>%
  separate(NAME,c("name","st"),sep = ",") %>%
  rename_all(tolower) %>%
  trim_census() %>%
  mutate(city_fips = as.numeric(geoid)) %>%
  select(city_fips, name, st, pop_2010 = value) %>%
  arrange(city_fips)
pa_ctys <- counties("PA",T,"20m") %>%
  mutate(cty_fips = as.numeric(GEOID)) %>%
  st_transform(.,crs = 3174) %>%
  select(cty_fips, geometry)
pa_centroid <- places("PA",T) %>%
  st_transform(.,crs = 3174) %>%
  st_centroid() %>%
  mutate(city_fips = as.numeric(GEOID)) %>%
  select(city_fips, geometry) %>%
  st_intersection(pa_ctys) %>%
  st_drop_geometry()
ny_univ <- ny_cities %>%
  mutate(title = paste(name,st,sep = ", ")) %>%
  left_join(.,hist_pops,by = 'title') %>%
  filter(!is.na(pop_1940)) %>%
  inner_join(ny_centroid, by = "city_fips") %>%
  inner_join(rucc, by = "cty_fips") %>%
  inner_join(cz, by = "cty_fips") %>%
  filter(pop_2010 >= 750,
         pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999 | pop_1910 > 999) %>%
  select(city_fips,city_name = name,st,cty_fips,rucc,cz,pop_2010) %>%
  arrange(city_name)
pa_univ <- pa_cities %>%
  mutate(title = paste(name,st,sep = ", ")) %>%
  left_join(.,hist_pops,by = 'title') %>%
  filter(!is.na(pop_1940)) %>%
  inner_join(pa_centroid, by = "city_fips") %>%
  inner_join(rucc, by = "cty_fips") %>%
  inner_join(cz, by = "cty_fips") %>%
  filter(pop_2010 >= 750,
         pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999 | pop_1910 > 999) %>%
  select(city_fips,city_name = name,st,cty_fips,rucc,cz,pop_2010) %>%
  arrange(city_name)
univ_nypa <- bind_rows(pa_univ,ny_univ) %>% 
  write_csv('data/univ_nypa.csv')

# rm(cz,hist_pops,ny_centroid,ny_ctys,pa_centroid,pa_ctys,rucc,info_clean,trim_census,ny_cities,pa_cities,pa_univ,ny_univ)

