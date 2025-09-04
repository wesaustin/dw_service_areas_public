################################################################################
# Comparing boundaries with EPIC boundary definitions
# National Center for Environmental Economics
# Latest update: 2/13/2023
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  stringr, # string manipulation
  tidycensus
)

################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

#WA
# my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

setwd(paste0(my_path))
getwd()

plot_path <- "ej_service_area/output/county maps/"

`%!in%` = Negate(`%in%`)

################################################################################
## Load geodata for all US
################################################################################

US_cbg_crop <- readRDS("Data/census_geo/US_cbg_crop.rds") 

US_st_crop <- readRDS("Data/census_geo/US_st_crop.rds")

################################################################################
## Load boundaries
################################################################################

# epic_hb_vio <- readRDS("Data/combined/HB_vio_epic_area.rds") 

usgs_hb_vio <- readRDS("Data/combined/HB_vio_usgs.rds")

zc_hb_vio <- readRDS("Data/combined/HB_vio_zc.rds")

hm_hb_vio <- readRDS("Data/combined/HB_vio_hm_area.rds")

################################################################################
# USGS Map
################################################################################

# Map 1 : HB violations at CBG

usgs_missing <- filter(hm_hb_vio, pwsid %!in% usgs_hb_vio$pwsid) %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE)

HB_vio_all <- left_join(US_cbg_crop, usgs_missing, by = "ID", relationship = "many-to-many")

HB_vio_plot <- ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = " ", palette = "Reds", 
                       direction = 1, 
                       # limits=c(0,10), 
                       #breaks = c(0,10), 
                       # labels = c(0,">10"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"usgs_comp.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

rm(usgs_hb_vio, usgs_missing)

################################################################################
# Zipcode Map
################################################################################

# Map 1 : HB violations at CBG

zc_missing <- filter(hm_hb_vio, pwsid %!in% zc_hb_vio$pwsid) %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE)

HB_vio_all <- left_join(US_cbg_crop, zc_missing, by = "ID", relationship = "many-to-many")

HB_vio_plot <- ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = " ", palette = "Reds", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"zc_comp.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

rm(zc_missing, zc_hb_vio)


