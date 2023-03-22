#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Robert Heilmayr
# Project: NWF - PY
# Date: 3/13/23
# Purpose: Explore deforestation in paraguay
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(units)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_dir <- 'remote/in/'

## Load Hansen data
load(paste0(data_dir, 'other_projects/data_paraguay.Rdata'))
defor_df <- dat

## Add Administrative boundaries
district_shp <- paste0(data_dir, 'other_projects/DISTRITOS_PARAGUAY_2012.shp')
district_sf <- st_read(district_shp)
district_sf <- district_sf %>% 
  clean_names()

## Chaco border
border_shp <- paste0(data_dir, 'other_projects/east_west_border.shp')
border_sf <- st_read(border_shp)

## Add slaughterhouses
sltr_csv <- paste0(data_dir, 'trase/PY_slaughterhouses_trase.csv')
sltr_sf <- read_csv(sltr_csv) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
  st_transform(st_crs(district_sf)) %>% 
  select(asset_id, geometry) %>% 
  distinct() %>% 
  arrange(asset_id)


## Define output directory
out_dir <- 'figures/'





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prep data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Restrict to recent deforestation
defor_df <- defor_df %>%
  filter(year > 2009)

## Convert to sf
defor_sf <- defor_df %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(4326))
defor_sf <- defor_sf %>% 
  st_transform(st_crs(district_sf))

## Add administrative boundaries data
defor_sf <- defor_sf %>% 
  st_join(district_sf)
chaco_dpto <- c("ALTO PARAGUAY", "BOQUERON", "PRESIDENTE HAYES")
defor_sf <- defor_sf %>% 
  mutate(chaco = case_when(
    dpto_desc %in% chaco_dpto ~ TRUE,
    !(dpto_desc %in% chaco_dpto) ~ FALSE,
    is.na(dpto_desc) ~ NA
  ))

## Create simplified points data with single row per observation
points_sf <- defor_sf %>% 
  filter(year == 2020) %>% 
  select(grid_cell_id)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate distances --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Biome border
nearest <- st_nearest_feature(points_df, border_sf)
dist <- st_distance(points_df, border_sf[nearest,], by_element=TRUE)
dist <- dist %>% 
  drop_units()
points_sf$biome_dist <- dist / 1000


# Slaughterhouses
nearest <- st_nearest_feature(points_df, sltr_sf)
dist <- st_distance(points_df, sltr_sf[nearest,], by_element=TRUE)
dist <- dist %>% 
drop_units()
points_sf$sltr_dist <- dist / 1000

defor_sf <- defor_sf %>% 
  left_join(points_sf %>% st_drop_geometry(), by = "grid_cell_id")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate deforestation pct --------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate total deforestation within period 
total_defor <- defor_sf %>%
  st_drop_geometry() %>% 
  group_by(grid_cell_id) %>% 
  summarise(total_defor = sum(annual_forest_loss_pixel_count))


total_defor <- defor_sf %>%
  # st_drop_geometry() %>% 
  filter(year == 2010) %>% 
  left_join(total_defor, by = "grid_cell_id") %>% 
  mutate(pct_defor = total_defor / treecover2000_count)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by administrative unit -----------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by distance to city --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_defor %>% 
  ggplot(aes(x = travel_time_city_city, y = pct_defor)) +
  geom_point()

dist_bin_df <- total_defor %>% 
  mutate(dist_bin = ntile(travel_time_city_city, 10)) %>% 
  group_by(dist_bin) %>% 
  summarize(pct_defor = mean(pct_defor))

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()


total_defor <- total_defor %>% 
  mutate(dist_bin = ntile(travel_time_city_city, 10))

dist_bin_total_defor <- total_defor %>% 
  group_by(dist_bin) %>% 
  summarize(total_defor = sum(annual_forest_loss_pixel_count))

dist_bin_df <- total_defor %>% 
  filter(year == 2010) %>% 
  group_by(dist_bin) %>% 
  summarise(treecover2000_count = sum(treecover2000_count)) %>% 
  left_join(dist_bin_total_defor, by = "dist_bin") %>% 
  mutate(pct_defor = total_defor / treecover2000_count)

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by suitability --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_defor <- total_defor %>% 
  mutate(dist_bin = ntile(gaez_attainable_yield_2000, 20))

dist_bin_total_defor <- total_defor %>% 
  group_by(dist_bin) %>% 
  summarize(total_defor = sum(annual_forest_loss_pixel_count))

dist_bin_df <- total_defor %>% 
  filter(year == 2010) %>% 
  group_by(dist_bin) %>% 
  summarise(treecover2000_count = sum(treecover2000_count)) %>% 
  left_join(dist_bin_total_defor, by = "dist_bin") %>% 
  mutate(pct_defor = total_defor / treecover2000_count)

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by distance to slaughterhouse -----------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_defor %>% 
  ggplot(aes(y = sltr_dist, x = travel_time_city_city)) +
  geom_point(alpha = 0.1) +
  geom_smooth()

total_defor %>% 
  filter(chaco==TRUE) %>% 
  ggplot(aes(x = sltr_dist, y = pct_defor)) +
  geom_point(alpha = 0.1) +
  geom_smooth()

dist_bin_df <- total_defor %>% 
  mutate(dist_bin = ntile(sltr_dist, 10)) %>% 
  group_by(dist_bin) %>% 
  summarize(pct_defor = mean(pct_defor),
            total_defor = sum(total_defor))

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = total_defor)) +
  geom_col()

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()

mod <- lm("pct_defor ~ sltr_dist + gaez_attainable_yield_2000 + travel_time_city_city", total_defor)
summary(mod)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by administrative region --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
defor_sf <- defor_sf %>% 
  left_join(biome_distance, by = "grid_cell_id") %>% 
  mutate(biome_dist = ifelse(chaco, -biome_dist, biome_dist))





defor_trends <- defor_sf %>% 
  filter(abs(biome_dist)<200) %>% 
  group_by(chaco, year) %>% 
  summarize(defor = sum(annual_forest_loss_pixel_count ),
            forest = sum(treecover_annual)) %>% 
  mutate(defor_pct = defor / forest)

defor_trends %>% 
  ggplot(aes(x = year, y = defor_pct, color = chaco)) +
  geom_line()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create some maps --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# rect_from_point <- function(x,xsize,ysize){
#   bbox <- st_bbox(x)
#   bbox <- bbox +c(xsize,0,0,-ysize)
#   return(st_as_sfc(bbox))
# }
# 
# rect_1km <- partial(rect_from_point, xsize = 10000, ysize = 10000)
# 
# 
# geoms <- total_defor$geometry
# geom <- rect_1km(geoms[1])
# geom <- geom %>% st_sf()
# geom$id <- 1
# 
# new_geoms <- geom
# for (i in 2:length(geoms)){
#   geom <- rect_1km(geoms[i])
#   geom <- geom %>% st_sf()
#   geom$id <- i
#   new_geoms = rbind(new_geoms, geom)
# }
# 
# test <- total_defor %>% 
#   st_drop_geometry() %>% 
#   cbind(new_geoms) %>% 
#   st_sf()
# 
# tmap_mode("view")
# tm_basemap("Stamen.Watercolor") +
#   tm_shape(test) + 
#   tm_polygons() +
#   tm_tiles("Stamen.TonerLabels")

test <- total_defor %>% 
  st_drop_geometry() 


%>% 
  st_set_geometry("new_geometry")

point <- total_defor[1] %>% pull(geometry)

convert_point

total_defor <- total_defor %>% arrange(desc(grid_cell_id))
tmap_mode("view")
tm_basemap("Stamen.TonerLite") +
  tm_shape(total_defor) + 
  tm_bubbles(col = "pct_defor", size = .1, border.col = NA, palette = "viridis")


tm_basemap("Stamen.TonerLite") +
  tm_shape(total_defor) + 
  tm_bubbles(col = "sltr_dist", size = .1, border.col = NA, palette = "viridis") +
  tm_tiles("Stamen.TonerLabels")


tm_basemap("Stamen.TonerLite") +
  tm_shape(total_defor) + 
  tm_bubbles(col = "travel_time_city_city", size = .1, border.col = NA, palette = "viridis") +
  tm_tiles("Stamen.TonerLabels")


tm_basemap("Stamen.TonerLite") +
  tm_shape(total_defor) + 
  tm_bubbles(col = "sltr_dist", size = .05, border.col = NA) +
  tm_tiles("Stamen.TonerLabels")


tm_basemap("Stamen.TonerLite") +
  tm_shape(total_defor) + 
  tm_bubbles(col = "sltr_dist", size = .1, border.col = NA, palette = "viridis") +
  tm_tiles("Stamen.TonerLabels")
