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
# library(comprehenr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out_dir <- 'remote/in/'
dir.create(file.path(out_dir, "other_projects"))

## Move Hansen data from LBCS project
lbcs_data_dir <- 'L:/Shared drives/emlab/projects/current-projects/land-based-solutions/data/processed/'
hansen_path <- paste0(lbcs_data_dir, 'data_paraguay.Rdata')
file.copy(hansen_path, paste0(out_dir, 'other_projects/'))

## Move administrative boundaries from NASA project
celpy_data_dir <- 'L:/Shared drives/emlab/projects/current-projects/cel-Paraguay-crops/Data/'
admin_dir <- paste0(celpy_data_dir,'01_Raw_data/Boundaries-Infrastructure/DEEGC/')
district_shp_list <- list.files(admin_dir, pattern="DISTRITOS")
# district_shp_list <- to_vec(for(path in district_shp_list) paste0(admin_dir, path))

for (file in district_shp_list) {
  file.copy(paste0(admin_dir, file), paste0(out_dir, 'other_projects/'))
}
