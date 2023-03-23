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
library(patchwork)

# set working directory --------------------------------------
wdir <- "remote/"

## read data -------------------------------------------------

# extracted mapbiomas annual lulc
py_mapb_lulc <- read_csv(paste0(wdir,"in/gee/py_random_pts_mapbiomas_classes.csv"))
# soy cover
py_soy <- read_csv(paste0(wdir,"in/gee/py_random_pts_annual_soy.csv"))
# gfc forest loss year
py_gfc_lossyr <- read_csv(paste0(wdir,"in/gee/py_random_pts_gfc_lossyear.csv"))

# py departments and districts
py_adm <- read_sf(paste0(wdir,"in/other_projects/DISTRITOS_PARAGUAY_2012.shp")) %>%
  select(dpto=DPTO_DESC,distrito=DIST_DESC)

## set crs ---------------------------------------------------

proj = "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"

## clean data ------------------------------------------------

# get departments and districts of each sample point
sample_adm <- py_soy %>%
  select(id=`system:index`,.geo) %>%
  mutate(coordinates = gsub('.*\\[(.*)\\][^\\[]*', '\\1', .geo, perl = TRUE)) %>%
  separate(coordinates, into =c("longitude","latitude"),sep=",") %>%
  select(-.geo) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = proj) %>%
  st_join(py_adm) %>%
  st_drop_geometry() 

# soy cover
py_soy_long <- py_soy %>%
  #filter(veg_mask_2009 == 1) %>% # filter to only areas starting with veg
  select(id=`system:index`,.geo,starts_with("soy")) %>%
  select(-.geo) %>%
  pivot_longer(cols = soy_2001:soy_2021,names_to = "first_yr_soy", values_to = "soy_cover") %>%
  mutate(first_yr_soy = str_sub(first_yr_soy, -4, -1)) %>%
  group_by(id) %>%
  slice(match(1,soy_cover)) %>%
  print()

# mapbiomas lulc in 2021
py_mapb_lulc_long <- py_lulc %>%
  select(id=id,.geo,starts_with("classification")) %>%
  select(-.geo) %>%
  pivot_longer(cols = classification_2010:classification_2021,names_to = "year", values_to = "mapb_class") %>%
  mutate(year = str_sub(year, -4, -1)) %>%
  filter(year == 2021) %>%
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

py_def_long <- py_gfc_lossyr %>%
  filter(!is.na(lossyear)) %>%
  select(id=`system:index`,lossyear) %>%
  mutate(lossyear = str_pad(lossyear, width=2, side="left", pad="0")) %>%
  mutate(first_yr_def = paste0(20,lossyear)) %>%
  select(-lossyear) %>%
  filter(first_yr_def > 2009)
  
py_lc_2021 <- py_soy %>%
  select(id=`system:index`,soy_2021)

# calculations ---------------------------------------------

## count of gap of years of deforestation and year of soy occurrence
py_soy_def_yr_diff <- py_def_long %>%
  #select(-deforestation) %>%
  left_join(select(py_soy_long,id,first_yr_soy),by="id") %>%
  mutate(first_yr_soy = as.numeric(first_yr_soy),
         first_yr_def = as.numeric(first_yr_def),
         first_yr_soy =ifelse(is.na(first_yr_soy),0,first_yr_soy),
         soy_def_yr_diff = ifelse(first_yr_soy > 0, first_yr_soy - first_yr_def,0)) %>%
  filter(first_yr_soy != 0) %>%
  left_join(sample_adm,by="id") %>%
  mutate(region = ifelse(dpto %in% c("ALTO PARAGUAY","BOQUERON","PRESIDENTE HAYES"),"CHACO","EASTERN PARAGUAY")) %>%
  group_by(soy_def_yr_diff,distrito,dpto,region) %>%
  summarize(n_samples =n()) %>%
  filter(soy_def_yr_diff >= 0) %>%
  print()

count_pos_neg_yr_samples <- py_soy_def_yr_diff %>%
  mutate(yr_diff_type = ifelse(soy_def_yr_diff >= 0,"positive","negative")) %>%
  group_by(yr_diff_type) %>%
  summarize(n_samples = sum(n_samples)) %>%
  print()

## count of samples deforested for soy/others based on mapB classes
py_yr_defor_type <- py_def_long %>%
  #select(-deforestation) %>%
  left_join(select(py_soy_long,id,first_yr_soy),by="id") %>%
  left_join(py_lc_2021,by="id") %>%
  mutate(type = ifelse(soy_2021 == 1,"Soy","Other"),
         first_yr_def = as.numeric(first_yr_def)) %>%
  left_join(select(py_mapb_lulc_long,id,mapb_class),by="id") %>%
  mutate(
    type1 = case_when(
      mapb_class == "15" & type != "Soy" ~ "Pasture",
      (mapb_class == "18" | mapb_class == 19 | mapb_class == 57 | mapb_class == 58) & type != "Soy"  ~ "Crops",
      mapb_class == "3" & type != "Soy" ~ "Vegetation",
      TRUE ~ type
    ) 
  ) %>%
  left_join(sample_adm,by="id") %>%
  drop_na(dpto) %>%
  group_by(first_yr_def,type1,distrito,dpto) %>%
  summarize(n_samples = n()) %>%
  print()

# create plots -----------------------------------------------

# set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed"),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(size = 12, color = "grey30",angle = 0),
                    axis.text.y = element_text(size = 12, color = "grey30"),
                    axis.title.x = element_text(size = 12, color = "grey30"),
                    axis.title.y = element_text(size = 12, color = "grey30"),
                    strip.text.x = element_text(size = 12, color="grey30"),
                    strip.background = element_rect(color=NA, fill=NA),
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 12,colour="grey30"),
                    legend.title = element_text(size = 12,colour="grey30"),
                    legend.position="bottom",
                    legend.direction="horizontal",
                    plot.margin=unit(c(0.1,0.1,0.1,0.1),"in"))

options(crayon.enabled = FALSE)


# plot 1: Lag between deforestation and soy establishment year (all departments)

p1 <- ggplot(data = py_soy_def_yr_diff, aes(x = soy_def_yr_diff, y = n_samples,fill=dpto,color=dpto)) + 
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks = seq(from = min(py_soy_def_yr_diff$soy_def_yr_diff), to = max(py_soy_def_yr_diff$soy_def_yr_diff), by =1),expand=c(0,0)) +
  geom_col(linewidth=0.1) +
  ylab("No of samples converted to soy\n") +
  xlab("Years after initial deforestation") +
  theme_plot +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                    '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bdbdbd',"#b15928")) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                    '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bdbdbd',"#b15928")) +
  guides(fill = guide_legend(title.position = "top",title="Department"),color=FALSE)

p1

ggsave(p1,file=paste0(wdir,"out\\plots\\soy_deforestation_gfc_lossyr_plot.png"), dpi=300, w=10, h=6, units = "in") 

# Plot 2: Lag between deforestation and soy establishment year (Chaco vs Non Chaco)

p2a <- ggplot(data = subset(py_soy_def_yr_diff,region == "CHACO"), aes(x = soy_def_yr_diff, y = n_samples,fill=dpto,color=dpto)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,250)) +
  scale_x_continuous(breaks = seq(from = min(py_soy_def_yr_diff$soy_def_yr_diff), to = max(py_soy_def_yr_diff$soy_def_yr_diff), by =1),expand=c(0,0)) +
  geom_col(linewidth=0.1) +
  ylab("No of samples\nconverted to soy") +
  xlab("Years after initial deforestation") +
  theme_plot +
  scale_fill_manual(values = c('#6a3d9a','#bdbdbd','#b15928')) +
  scale_color_manual(values = c('#6a3d9a','#bdbdbd','#b15928')) +
  guides(fill = guide_legend(title.position = "top",title="Department"),color = FALSE) +
  facet_grid(~region)

p2a

p2b <- ggplot(data = subset(py_soy_def_yr_diff,region != "CHACO"), aes(x = soy_def_yr_diff, y = n_samples,fill=dpto,color=dpto)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,250)) +
  scale_x_continuous(breaks = seq(from = min(py_soy_def_yr_diff$soy_def_yr_diff), to = max(py_soy_def_yr_diff$soy_def_yr_diff), by =1),expand=c(0,0)) +
  geom_col(linewidth=0.1) +
  ylab("No of samples\nconverted to soy") +
  xlab("Years after initial deforestation") +
  theme_plot +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                   '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bdbdbd',"#b15928")) +
                   scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
                    '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#bdbdbd',"#b15928")) +
                      guides(fill = guide_legend(title.position = "top",title="Department"),color = FALSE) +
  facet_grid(~region)

p2b

p2 <- p2a / p2b
p2

ggsave(p2,file=paste0(wdir,"out\\plots\\soy_deforestation_gfc_lossyr_chaco_others_plot.png"), dpi=300, w=7.5, h=10, units = "in") 


# plot 3: Deforestation land cover type by year

p3 <- ggplot(data = py_yr_defor_type, aes(x = first_yr_def, y = n_samples,fill=type,color=type)) + 
  scale_x_continuous(breaks=seq(from=2010,to=2021,by=1)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_col(linewidth=0.1) +
  ylab("No of samples\n") +
  xlab("Year") +
  theme_plot +
  scale_fill_manual(values = c('#a6cee3','#fb9a99')) +
  scale_color_manual(values = c('#a6cee3','#fb9a99')) +
  guides(fill = guide_legend(title.position = "top",title="Land cover in 2021"),color=FALSE)

p3

ggsave(p3,file=paste0(wdir,"out\\plots\\deforestation_type_plot.png"), dpi=300, w=8, h=5, units = "in") 
