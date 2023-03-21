## ---------------------------------------------------------
##
## Project: Paraguay NWF
##
## Purpose of script: Plot areas of Forest, OWL and Neither within Paraguayan Chaco
##
## Author: Jason Benedict
##
## Date Created: 2023-03-21
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
library(d3.format)

# set working directory --------------------------------------
wdir <- "remote/"

## read data -------------------------------------------------

# classified area classes
lc_class <- read_csv(paste0(wdir,"in/gee/class_area_by_distrito.csv"))

# py departments and districts
py_adm <- read_sf(paste0(wdir,"in/other_projects/DISTRITOS_PARAGUAY_2012.shp")) %>%
  #select(dpto=DPTO_DESC,distrito=DIST_DESC) %>%
  mutate(ID = paste0(DPTO,DISTRITO))

## clean and merge data --------------------------------------

lc_class_long <- lc_class %>%
  pivot_longer(cols = `0`:`3`,names_to = "CLASS", values_to = "AREA_HA") %>%
  left_join(py_adm,by="ID") %>%
  st_drop_geometry() %>%
  mutate(REGION = ifelse(DPTO_DESC %in% c("ALTO PARAGUAY","BOQUERON","PRESIDENTE HAYES"),"CHACO","OTHERS"),
         CLASS_DESC = case_when(CLASS == 0 ~ "Neither",
                                CLASS == 1 ~ "Other Wooded Lands (OWL)",
                                CLASS == 2 | CLASS == 3 ~ "Forest",
                                TRUE ~ NA)) %>%
  select(ID,DPTO,DISTRITO,DPTO_DESC,DIST_DESC,REGION,CLASS_DESC,AREA_HA) %>%
  filter(REGION == "CHACO")

## plot -----------------------------------------------------

# set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.x = element_line(color="grey70",linetype="dashed",linewidth=0.35),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(size = 22, color = "grey30",angle = 0, face="bold"),
                    axis.text.y = element_text(size = 22, color = "grey30"),
                    axis.title.x = element_text(size = 22, color = "grey30"),
                    axis.title.y = element_text(size = 22, color = "grey30"),
                    strip.text.x = element_text(size = 25, face = "bold",color="grey30"),
                    strip.background = element_rect(color=NA, fill=NA),
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 24,colour="grey30"),
                    legend.title = element_text(size = 20,colour="grey30"),
                    legend.position="bottom",
                    legend.direction="horizontal",
                    plot.margin=unit(c(0.1,0.1,0.1,0.1),"in"))

options(crayon.enabled = FALSE)

p1 <- ggplot(data = lc_class_long, aes(x = AREA_HA, y = reorder(DIST_DESC, AREA_HA), 
     fill=factor(CLASS_DESC, levels=c("Forest", "Other Wooded Lands (OWL)", "Neither")), 
     color = factor(CLASS_DESC, levels=c("Forest", "Other Wooded Lands (OWL)", "Neither")))) + 
  scale_x_continuous(labels = d3_format(".2~s"),expand=c(0,0)) +
  geom_col(linewidth=0.1) +
  xlab("Area (ha)") +
  ylab("Distritos") +
  theme_plot +
  scale_fill_manual(values = c('#33a02c','#b15928','#a6cee3')) +
  scale_color_manual(values = c('#33a02c','#b15928','#a6cee3')) +
                                          guides(fill = guide_legend(title.position = "top",title=""),
                                                 color=FALSE) +
  facet_wrap(DPTO_DESC~.,nrow=3,scales="free_y") 

p1

ggsave(p1,file=paste0(wdir,"out\\plots\\chaco_forest_owl_areas_plot.png"), dpi=300, w=8, h=6, units = "in",type = "cairo-png") 
