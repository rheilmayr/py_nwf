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
library(sf)
library(janitor)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load Hansen data
lbcs_data_dir <- 'L:/Shared drives/emlab/projects/current-projects/land-based-solutions/data/processed/'
load(paste0(lbcs_data_dir, 'data_paraguay.Rdata'))
defor_df <- dat

## Add Administrative boundaries
celpy_data_dir <- 'L:/Shared drives/emlab/projects/current-projects/cel-Paraguay-crops/Data/'
district_shp <- paste0(celpy_data_dir, '01_Raw_data/Boundaries-Infrastructure/DEEGC/DISTRITOS_PARAGUAY_2012.shp')
district_sf <- st_read(district_shp)
district_sf <- district_sf %>% 
  clean_names()

## Add slaughterhouses


## Define output directory
out_dir <- 'figures/'





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prep data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Restrict to recent deforestation
# defor_df <- defor_df %>% 
#   filter(year > 2009)

total_defor <- defor_df %>% 
  group_by(grid_cell_id) %>% 
  summarise(total_defor = sum(annual_forest_loss_pixel_count))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by distance --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dist_df <- defor_df %>% 
  filter(year == 2010) %>% 
  left_join(total_defor, by = "grid_cell_id") %>% 
  mutate(pct_defor = total_defor / treecover2000_count)

dist_df %>% 
  ggplot(aes(x = travel_time_city_city, y = pct_defor)) +
  geom_point()

dist_bin_df <- dist_df %>% 
  mutate(dist_bin = ntile(travel_time_city_city, 10)) %>% 
  group_by(dist_bin) %>% 
  summarize(pct_defor = mean(pct_defor))

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()


defor_df <- defor_df %>% 
  mutate(dist_bin = ntile(travel_time_city_city, 10))

dist_bin_total_defor <- defor_df %>% 
  group_by(dist_bin) %>% 
  summarize(total_defor = sum(annual_forest_loss_pixel_count))

dist_bin_df <- defor_df %>% 
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
defor_df <- defor_df %>% 
  mutate(dist_bin = ntile(gaez_attainable_yield_2000, 20))

dist_bin_total_defor <- defor_df %>% 
  group_by(dist_bin) %>% 
  summarize(total_defor = sum(annual_forest_loss_pixel_count))

dist_bin_df <- defor_df %>% 
  filter(year == 2010) %>% 
  group_by(dist_bin) %>% 
  summarise(treecover2000_count = sum(treecover2000_count)) %>% 
  left_join(dist_bin_total_defor, by = "dist_bin") %>% 
  mutate(pct_defor = total_defor / treecover2000_count)

dist_bin_df %>% 
  ggplot(aes(x = dist_bin, y = pct_defor)) +
  geom_point()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deforestation by administrative region --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
defor_sf <- defor_df %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(4326))

defor_sf <- defor_sf %>% 
  st_transform(st_crs(district_sf))

defor_sf <- defor_sf %>% 
  st_join(district_sf)

chaco_dpto <- c("ALTO PARAGUAY", "BOQUERON", "PRESIDENTE HAYES")
defor_sf <- defor_sf %>% 
  mutate(chaco = case_when(
    dpto_desc %in% chaco_dpto ~ TRUE,
    !(dpto_desc %in% chaco_dpto) ~ FALSE,
    is.na(dpto_desc) ~ NA
  ))

defor_trends <- defor_sf %>% 
  group_by(chaco, year) %>% 
  summarize(defor = sum(annual_forest_loss_pixel_count ),
            forest = sum(treecover_annual)) %>% 
  mutate(defor_pct = defor / forest)

defor_trends %>% 
  ggplot(aes(x = year, y = defor_pct, color = chaco)) +
  geom_line()
