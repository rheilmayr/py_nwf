## ---------------------------------------------------------
##
## Project: Paraguay NWF
##
## Purpose of script: 
##
## Author: Jason Benedict
##
## Date Created: 2023-03-16
## 
## ---------------------------------------------------------
##
## Notes: 
##
##   
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

## load packages
library(tidyverse)
library(readxl)
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(sf)
library(dlookr)
library(khroma)

# set working directory --------------------------------------
wdir <- "remote/in/"

## read data -------------------------------------------------

# extracted soy annual cover and mapbiomas annual deforestation
py_soy_def <- read_csv(paste0(wdir,"gee/py_random_pts_annual_soy_expansion_mapb_def_chaco.csv"))
# py departments and districts
py_adm <- read_sf(paste0(wdir,"other_projects/DISTRITOS_PARAGUAY_2012.shp")) %>%
  select(dpto=DPTO_DESC,distrito=DIST_DESC)

## set crs ---------------------------------------------------

proj = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"

## clean data ------------------------------------------------

# get departments and districts of each sample point
sample_adm <- py_soy_def %>%
  select(id=`system:index`,.geo) %>%
  mutate(coordinates = gsub('.*\\[(.*)\\][^\\[]*', '\\1', .geo, perl = TRUE)) %>%
  separate(coordinates, into =c("longitude","latitude"),sep=",") %>%
  select(-.geo) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = proj) %>%
  st_join(py_adm) %>%
  st_drop_geometry() 

# soy cover
py_soy_long <- py_soy_def %>%
  filter(veg_mask_2009 == 1) %>% # filter to only areas starting with veg
  select(id=`system:index`,.geo,starts_with("soy")) %>%
  select(-.geo) %>%
  pivot_longer(cols = soy_2001:soy_2021,names_to = "first_yr_soy", values_to = "soy_cover") %>%
  mutate(first_yr_soy = str_sub(first_yr_soy, -4, -1)) %>%
  group_by(id) %>%
  slice(match(1,soy_cover)) %>%
  print()

# deforestation
py_def_long <- py_soy_def %>%
  filter(veg_mask_2009 == 1) %>% # filter to only areas starting with veg
  select(id=`system:index`,.geo,starts_with("deforestation")) %>%
  select(-.geo) %>%
  pivot_longer(cols = deforestation_2010:deforestation_2021,names_to = "first_yr_def", values_to = "deforestation") %>%
  mutate(first_yr_def = str_sub(first_yr_def, -4, -1)) %>%
  group_by(id) %>%
  slice(match(1,deforestation)) %>%
  print()

py_lc_2021 <- py_soy_def %>%
  select(id=`system:index`,soy_2021)

# calculations ---------------------------------------------

## count of gap of years of deforestation and year of soy occurrence
py_soy_def_yr_diff <- py_def_long %>%
  select(-deforestation) %>%
  left_join(select(py_soy_long,id,first_yr_soy),by="id") %>%
  mutate(first_yr_soy = as.numeric(first_yr_soy),
         first_yr_def = as.numeric(first_yr_def),
         first_yr_soy =ifelse(is.na(first_yr_soy),0,first_yr_soy),
         soy_def_yr_diff = ifelse(first_yr_soy > 0, first_yr_soy - first_yr_def,0)) %>%
  filter(first_yr_soy != 0) %>%
  left_join(sample_adm,by="id") %>%
  group_by(soy_def_yr_diff,distrito,dpto) %>%
  summarize(n_samples =n()) %>%
  print()

count_pos_neg_yr_samples <- py_soy_def_yr_diff %>%
  mutate(yr_diff_type = ifelse(soy_def_yr_diff > 0,"positive","negative")) %>%
  group_by(yr_diff_type) %>%
  summarize(n_samples = sum(n_samples)) %>%
  print()

## count of samples deforested for soy/others
py_yr_defor_type <- py_def_long %>%
  select(-deforestation) %>%
  left_join(select(py_soy_long,id,first_yr_soy),by="id") %>%
  left_join(py_lc_2021,by="id") %>%
  mutate(type = ifelse(soy_2021 == 1,"Soy","Other"),
         first_yr_def = as.numeric(first_yr_def)) %>%
  left_join(sample_adm,by="id") %>%
  group_by(first_yr_def,type,distrito,dpto) %>%
  summarize(n_samples = n()) %>%
  print()

# create plots -----------------------------------------------

# set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed",linewidth=0.35),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(size = 8, color = "grey30",angle = 0, face="bold"),
                    axis.text.y = element_text(size = 9, color = "grey30"),
                    axis.title.x = element_text(size = 10, color = "grey30"),
                    axis.title.y = element_text(size = 10, color = "grey30"),
                    strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                    strip.background = element_rect(color=NA, fill=NA),
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 7,colour="grey30"),
                    legend.title = element_text(size = 9,colour="grey30"),
                    legend.position="bottom",
                    legend.direction="horizontal",
                    plot.margin=unit(c(0.1,1.5,0.1,0.5),"cm"))

options(crayon.enabled = FALSE)


# plot 1: Lag between deforestation and soy establishment year

p1 <- ggplot(data = py_soy_def_yr_diff, aes(x = soy_def_yr_diff, y = n_samples,fill=dpto)) + 
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks = seq(from = min(py_soy_def_yr_diff$soy_def_yr_diff), to = max(py_soy_def_yr_diff$soy_def_yr_diff), by =1),expand=c(0,0)) +
  geom_col() +
  ylab("No of samples converted to soy\n") +
  xlab("\nYears after initial deforestation") +
  theme_plot +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                    '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bdbdbd')) +
  guides(fill = guide_legend(title.position = "top",title="Department"))

p1

# plot 2: Deforestation land cover type by year

p2 <- ggplot(data = py_yr_defor_type, aes(x = first_yr_def, y = n_samples,fill=type)) + 
  scale_x_continuous(breaks=seq(from=2010,to=2021,by=1)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_col() +
  ylab("No of samples\n") +
  xlab("\nYear") +
  theme_plot +
  scale_fill_manual(values = c('#a6cee3','#fb9a99')) +
  guides(fill = guide_legend(title.position = "top",title="Land cover in 2021"))

p2
