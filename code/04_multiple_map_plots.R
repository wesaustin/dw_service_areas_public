

################################################################################
# Plotting multiple indicator plots
# National Center for Environmental Economics
# Latest update: 8/31/2023
################################################################################


################################################################################
## Load packages: 
################################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot,
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  MASS
)

################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

#WA
#my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

setwd(paste0(my_path))
getwd()

plot_path <- "ej_service_areas/output/state/"


################################################################################
## Load data: 
################################################################################

cbg_HB_vio <- read_rds("Data/combined/hb_vio_hm_area.rds")


################################################################################
##Grab TIGRIS boundaries for area of interest:  Run for NJ
################################################################################

NJ_bg <- tigris::block_groups("NJ", cb = TRUE) %>%
  mutate(ID=str_pad(GEOID, 12, pad="0")) %>%
  st_transform(crs = 4326)  

NJ_tract <- tigris::tracts("NJ", cb = TRUE) %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

NJ_counties <- tigris::counties("NJ", cb = TRUE) %>%
  mutate(county = str_pad(GEOID, 5, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(NJ_bg)
st_crs(NJ_tract)

################################################################################
## Plot at administrative divisions to compare : Run for NJ 
################################################################################

##Map 1 : Block group

cbg_vio_NJ <- cbg_HB_vio %>%
  filter(st_abbrev == "NJ") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = mean(total_violations)) %>%
  mutate(avg_vio_length = mean(diff_days_max)) %>%
  distinct(ID, .keep_all = TRUE)


HB_vio_NJ <- left_join(NJ_bg, cbg_vio_NJ)

HB_NJ_bg <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = " ", palette = "Reds", 
                       limits=c(0,28), breaks = c(0,10,20,28), 
                       labels = c(0,10,20,28), 
                       direction = 1, na.value = scales::alpha("#DCDCDC", 0.2)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank())


png(file = paste0(plot_path,"NJ_hb_vio_cbg.png"), 
    width = 691, height = 924, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_NJ_bg)

dev.off()


# Map 2 - Tract 

tract_vio_NJ <- cbg_HB_vio %>%
  filter(st_abbrev == "NJ")  %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*pop_sum)/sum(pop_sum)) #average violations per tract

HB_vio_NJ <- left_join(NJ_tract, tract_vio_NJ) #join by tract

HB_NJ_tract <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = " ", palette = "Reds", 
                       limits=c(0,28), breaks = c(0,10,20,28), 
                       labels = c(0,10,20,28), 
                       direction = 1, na.value = scales::alpha("#DCDCDC", 0.2)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank())


png(file = paste0(plot_path,"NJ_hb_vio_tract.png"), 
    width = 691, height = 924, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_NJ_tract)

dev.off()

## Map 3 : County

county_vio_NJ <- cbg_HB_vio %>%
  filter(st_abbrev == "NJ") %>%
  mutate(county = substring(ID, first=1, last=5))  %>%
  group_by(county) %>%
  mutate(avg_vio_county = sum(total_violations*pop_sum)/sum(pop_sum))

HB_vio_NJ <- left_join(NJ_counties, county_vio_NJ)

st_as_sf(HB_vio_NJ)

HB_NJ_county <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_county, geometry = geometry), color = NA) +
  scale_fill_distiller(name = " ", palette = "Reds", 
                       limits=c(0,10), breaks = c(0,4,7,10), 
                       labels = c(0,4,7,10), 
                       direction = 1, na.value = scales::alpha("#DCDCDC", 0.2)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank())



png(file = paste0(plot_path,"NJ_hb_vio_county.png"), 
    width = 691, height = 924, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_NJ_county)

dev.off()

library(patchwork)

patchwork <- HB_NJ_bg + HB_NJ_tract + HB_NJ_county +  
  plot_layout(ncol = 2)

ggsave2(filename = 'NJ_HB_maps.png', path = plot_path)

rm(HB_vio_NJ, HB_NJ_bg, HB_NJ_tract, HB_NJ_county, NJ_bg, NJ_counties, NJ_tract, patchwork)


patchwork + plot_annotation(
  title = 'New Jersey Health-based Violations',
  caption = 'Comparison of violations for different Census levels',
  theme = theme(plot.title = element_text(family = "serif", size = 12), plot.caption = element_text(family = "serif", size = 8))
)


