library(SpatialKDE)
library(sf)
library(viridis)
library(tigris)
library(rleuven)
library(tidyverse)
library(ggpubr)

# Read in data ------------------------------------------------------------
df <- downtownR::ny_all %>%
  filter(cty_fips == 36089)
ny_univ <- df %>% select(city_fips,city_name,pop_2010,rucc,cz) %>% distinct() %>%
  mutate(st = "NY")

# D3 Setup ----------------------------------------------------------------
# town_fips <- ny_univ %>% filter(pop_2010 > 10000, rucc > 3) %>% sample_n(1) %>% pull(city_fips)
# town_fips <- 3638264
town_fips <- 3646019
town_name <- ny_univ %>% filter(city_fips == town_fips) %>%
  mutate(name = paste0(city_name,", ",st)) %>% pull(name)
st <- str_sub(town_fips,end = 2)
town_sf <- places(st = st, cb = T) %>%
  select(city_fips = GEOID,geometry) %>%
  filter(city_fips == town_fips) %>%
  st_transform(.,crs = 3174)
town_dots_all <- df %>% filter(city_fips == town_fips) %>%
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>%
  st_transform(.,crs = 3174)
town_dots <- town_dots_all %>%
  # filter(naics3 %in% c(921,541,524,722,811,812,813,624,522,711,712)) %>%
  select(naics3,geometry)
cnty <- counties(st, cb = T) %>% st_transform(3174) %>%
  filter(st_intersects(x = ., y = town_sf, sparse = FALSE)) %>% pull(GEOID) %>% str_sub(.,start = -3)
town_roads <- roads(st = st, county = cnty) %>%
  st_transform(.,crs = 3174) %>%
  st_intersection(town_sf,.) %>%
  st_collection_extract(type = "LINESTRING")
town_h2o <- area_water(st = st, county = cnty) %>%
  st_transform(.,crs = 3174) %>%
  st_buffer(15) %>%
  st_intersection(town_sf,.)

cell_size <- 100
band_width <- 150

solo_grid <- st_make_grid(town_sf, cellsize = cell_size, square = F) %>%
  st_intersection(town_sf)
# solo_grid_sq <- st_make_grid(town_sf, cellsize = cell_size, square = T) %>%
#   st_intersection(town_sf)
#
# (p1 <- ggplot() +
#   geom_sf(data = solo_grid_sq, alpha = 0, size = .4, color = "gray70") +
#   geom_sf(data = town_sf, alpha = 0, size = 1) +
#   theme_void())
#
# (p2 <- ggplot() +
#   geom_sf(data = solo_grid, alpha = 0, size = .4, color = "gray70") +
#   geom_sf(data = town_sf, alpha = 0, size = 1) +
#   theme_void())
#
# ggarrange(p1,p2,ncol=2) +
#   ggsave("data-raw/grid_compare.png")


# D3 Step 1 ---------------------------------------------------------------
bfam <- 'LM Roman 10'
bsize <- 9
tface <- 'plain'

(stp1aa <- ggplot() +
   geom_sf(data = solo_grid, alpha = 0, size = .4, color = "gray70") +
   geom_sf(data = town_sf, alpha = 0) +
   theme_void(base_family = bfam, base_size = bsize) +
   theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
   labs(title = 'Hexagonal Grid Plotted'))

(stp1a <- ggplot() +
  geom_sf(data = town_sf, fill = "yellow", alpha = .15) +
  geom_sf(data = solo_grid, alpha = 0, size = .4, color = "gray70") +
  geom_sf(data = town_dots_all, size = .5)+
  geom_sf(data = town_sf, alpha = 0) +
  theme_void(base_family = bfam, base_size = bsize) +
  labs(title = 'All Establishments Plotted') +
  theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step1a.png", width = 9, height = 9))

town_kde <- town_dots %>%
  kde(band_width = band_width, kernel = "epanechnikov", grid = town_dots %>%
        create_grid_hexagonal(cell_size = cell_size,
                              side_offset = band_width)) %>%
  st_intersection(town_sf,.) %>%
  mutate(z = round((kde_value - mean(kde_value)) / sd(kde_value),2))

(stp1b <- ggplot() +
  geom_sf(data = town_sf, fill = "#FCFDBF") +
  geom_sf(data = town_kde, aes(fill = z), color = NA) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  # geom_sf(data = town_dots_all, size = .5, stroke = .5,
  #         shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
  theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  labs(title = 'Business Establishment Density Plotted') +
  # labs(fill = "Kernel Density Z-Score") + #caption = "Source: Infogroup, 2005",
  ggsave("data-raw/step1b.png", width = 9, height = 9))


# D3 Step 2 ---------------------------------------------------------------

town_kde2a <- town_kde %>%
  filter(kde_value > 0)

# dist_wzeros <- ggplot() +
#  geom_histogram(data = town_kde, alpha = .6, bins = 100L, fill = "#bb0000", aes(x = kde_value), color = 'black') +
#  labs(x = "Establishment Density", y = "Number of Cells") +
#  theme_minimal(base_size = 10, base_family = bfam, base_size = bsize) + ylim(0, 825) + xlim(-1,36)
# dist_wo_zeros <- ggplot() +
#  geom_histogram(data = town_kde2a, alpha = .6, bins = 100L, fill = "#003399", aes(x = kde_value), color = 'black') +
#  labs(x = "Establishment Density", y = "Number of Cells") +
#  theme_minimal(base_size = 10, base_family = bfam, base_size = bsize) + ylim(0, 825) + xlim(-1,36)
#
# ggarrange(dist_wzeros,dist_wo_zeros, ncol = 2,label.y = 1) +
#   ggsave("data-raw/dist_compare.png", width = 10, height = 6)
#
# mean(town_kde$kde_value)
# mean(town_kde2a$kde_value)
# sd(town_kde$kde_value)
# sd(town_kde2a$kde_value)


kde_polygons2a <- st_dissolve(town_kde2a)

(stp2a <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = town_kde2a, aes(fill = kde_value), color = NA) +
  geom_sf(data = kde_polygons2a, alpha = 0, color = "black", size = .2) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
    labs(title = 'Zero-Density Cells Removed') +
    theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step2a.png", width = 9, height = 9))

town_kde2b <- town_kde2a %>%
  mutate(tile = ntile(kde_value,100)) %>%
  arrange(desc(kde_value)) %>%
  # filter(top_sd == 1)
  filter(z >= 2)

# dist_filtered <- ggplot() +
#   geom_histogram(data = town_kde2b, alpha = .6, bins = 100L, fill = "yellow", aes(x = kde_value), color = 'black') +
#   labs(x = "Establishment Density", y = "Number of Cells") +
#   theme_minimal(base_size = 10, base_family = bfam, base_size = bsize) + ylim(0, 825) + xlim(-1,36)
# mean(town_kde2b$kde_value)
# sd(town_kde2b$kde_value)
#
# ggarrange(dist_wzeros,dist_wo_zeros,dist_filtered, ncol = 3) #+ ggsave("data-raw/dist_compare.png", width = 10, height = 6)


kde_polygons2b <- st_dissolve(town_kde2b)

(stp2b <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = town_kde2b, aes(fill = kde_value), color = NA) +
  geom_sf(data = kde_polygons2b, alpha = 0, color = "black", size = .2) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
    labs(title = 'Highest Density Cells Only') +
    theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step2b.png", width = 9, height = 9))

kde_blob2 <- st_dissolve(town_kde2b) %>%
  select(-STATUS) %>%
  st_cast(.,"POLYGON") %>%
  st_dissolve() %>%
  st_buffer(30, joinStyle = "MITRE", endCapStyle = "SQUARE") %>%
  smoothr::smooth(.,  method = "ksmooth", smoothness = 5) %>%
  mutate(id = town_fips) %>%
  select(id,geometry)

(stp2c <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = kde_blob2, fill = "black", alpha = .3) +
  geom_sf(data = town_kde2b, aes(fill = kde_value), color = NA, alpha = .4) +
  geom_sf(data = kde_polygons2b, alpha = 0, color = "black", size = .2) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
    labs(title = 'Cluster Polygons Generated') +
    theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step2c.png", width = 9, height = 9))


# D3 Step 3 ---------------------------------------------------------------

kde_blob3 <- st_dissolve(town_kde2b) %>%
  select(-STATUS) %>%
  st_cast(.,"POLYGON") %>%
  mutate(area = st_area(.)) %>%
  mutate(rank = dense_rank(desc(area))) %>%
  st_intersection(.,town_kde) %>%
  group_by(rank) %>%
  mutate(mean_top90 = round(mean(kde_value[kde_value>=quantile(kde_value, 0.9)]),1)) %>%
  ungroup() %>%
  filter(mean_top90 > max(mean_top90)/2.5,
         area > max(area/2)) %>%
  st_dissolve() %>%
  st_buffer(30, joinStyle = "MITRE", endCapStyle = "SQUARE") %>%
  smoothr::smooth(.,  method = "ksmooth", smoothness = 5) %>%
  mutate(id = town_fips) %>%
  select(id,geometry)

(stp3a <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = kde_blob3, fill = "black", alpha = .3) +
  geom_sf(data = town_kde2b, aes(fill = kde_value), color = NA, alpha = .4) +
  geom_sf(data = kde_polygons2b, alpha = 0, color = "black", size = .2) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
  labs(title = 'Downtown Cluster Selected') +
  theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step3a.png", width = 9, height = 9))

(stp3b <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = kde_blob3 %>% st_buffer(100), fill = "black", alpha = .3) +
  geom_sf(data = town_kde2b, aes(fill = kde_value), color = NA, alpha = .4) +
  geom_sf(data = kde_polygons2b, alpha = 0, color = "black", size = .2) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
  labs(title = 'Buffer Operation (100m)') +
  theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step3b.png", width = 9, height = 9))

(stp3c <- ggplot() +
  geom_sf(data = town_sf, fill = "gray95") +
  geom_sf(data = town_h2o, color = "black", fill = "#7BAFD4", alpha = .7, size = .2) +
  geom_sf(data = kde_blob3 %>% st_buffer(100), fill = "black", alpha = .3) +
  geom_sf(data = town_roads, alpha = .5, size = .5) +
  geom_sf(data = town_dots_all, size = .25, stroke = .25,
          shape = 21, fill = "white", alpha = 1, color = "black")+
  geom_sf(data = town_sf, alpha = 0) +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  theme_void(base_family = bfam, base_size = bsize) +
  labs(title = 'Final Downtown District Delineation') +
  theme(legend.position="none",plot.title = element_text(hjust = .5, face = tface)) +
  ggsave("data-raw/step3c.png", width = 9, height = 9))

stps_grid <- ggarrange(stp1aa,stp1a,stp1b,stp2a,stp2b,stp2c,stp3a,stp3b,stp3c,ncol = 3, nrow = 3) +
  ggsave('data-raw/plot3by3_alt.png', width = 11, height = 8.5)

beepr::beep()

kde_blob3 %>% st_buffer(-10) %>% st_write('data/massena2.kml')
