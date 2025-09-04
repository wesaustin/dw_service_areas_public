


################################################################################
# Bivariate maps for drinking water indicators : country maps
# National Center for Environmental Economics
# Last edited: 10/6/30
################################################################################

# Drawing from code developed by Wes Austin for a biscale function with action levels 

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf,
  cowplot,
  patchwork,
  dplyr, # data wrangling
  mapview,
  ggplot2,
  tidycensus, #Census data
  readr,
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot, # for bivariate mapping
  stringr, # string manipulation
  mapview,
  pals
)

# library(remotes)
# 
# remotes::install_github("chris-prener/biscale", force = TRUE)
library(biscale)

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "ej_service_areas/output/state bivariate maps/"

# Source code for bi_scale manipulations

source('ej_service_areas/code/biscale_fn_action_level.R')

custom_pal = c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#d38d82",
  "3-1" = "#d3250b", # high x, low y
  "1-2" = "#7eb6c3",
  "2-2" = "#7e7a78", # medium x, medium y
  "3-2" = "#7e200a",
  "1-3" = "#1e95b1", # low x, high y
  "2-3" = "#1e646d",
  "3-3" = "#1e1a09"  # high x, high y
)


################################################################################
## Health violations w/ HM boundaries
################################################################################

cbg_HB_vio <- readRDS("data/combined/HB_vio_hm_area.rds") %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = mean(total_violations, na.rm = TRUE)) %>%
  mutate(avg_vio_length = mean(diff_days_max)) %>%
  distinct(ID, .keep_all = TRUE) 

#205662 obs

# HB_vio_all <- left_join(US_cbg_crop, cbg_HB_vio) 
# 
# st_as_sf(HB_vio_all)


################################################################################
## Create action level dataset
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(cbg_HB_vio$avg_vio_cbg)

vTert = quantile(cbg_HB_vio$avg_vio_cbg, c(0:3/3))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    0.00    0.93    0.50  176.00   77727 

mTert = quantile(cbg_HB_vio$minorpct, c(0:3/3))

# 0.0000000 0.1588474 0.4898225 1.0000000

#3rd quartile = 0.5, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for indicator-specific

## Race

#Establish biscale comparison: HB violations and % POC

data_biscale_al <- bi_class_al(cbg_HB_vio,
                               x = avg_vio_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

################################################################################
## Subset state analysis: Oklahoma
################################################################################

OK_bg <- tigris::block_groups("OK") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

OK_cnty <- tigris::counties("OK") %>%
  st_transform(crs = 4326) 

hb_vio_OK <- left_join(OK_bg, data_biscale_al)

summary(hb_vio_OK$minorpct)

# Map 1: Health based violations and % POC

map <- ggplot() +
  geom_sf(data = hb_vio_OK, mapping = aes(fill = bi_class), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  geom_sf(data = OK_cnty, fill = NA, color = "#969696") +
  theme(plot.background = element_blank()) 

breaks <- list(bi_x = c("0", "1", "2", "176"),
               bi_y = c("0", "0.21", "0.46", "1"))

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% People of color →",
                    size = 50,
                    breaks = breaks,
                    arrows = FALSE,
                    base_family = "serif")


# Plot and legend

hb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.005, y = 0.15, width = 0.4, height = 0.4)


png(file = paste0(plot_path,"hb_POC_biv_OK_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(hb_minor)

dev.off()


################################################################################
## Health violations w/ epic boundaries
################################################################################

cbg_HB_vio <- readRDS("Data/combined/HB_vio_epic_area.rds") %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = mean(total_violations, na.rm = TRUE)) %>%
  mutate(avg_vio_length = mean(diff_days_max)) %>%
  distinct(ID, .keep_all = TRUE) 

#205662 obs

# HB_vio_all <- left_join(US_cbg_crop, cbg_HB_vio) 
# 
# st_as_sf(HB_vio_all)


################################################################################
## Create action level dataset
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(cbg_HB_vio$avg_vio_cbg)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    0.00    0.93    0.50  176.00   77727 

vTert = quantile(cbg_HB_vio$avg_vio_cbg, c(0:3/3))
# 0.0000000   0.0000000   0.3333333 366.0000000 

mTert = quantile(cbg_HB_vio$minorpct, c(0:3/3))

# 0.0000000 0.1588474 0.4898225 1.0000000

#3rd quartile = 0.5, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for indicator-specific

## Race

#Establish biscale comparison: HB violations and % POC

data_biscale_al <- bi_class_al(cbg_HB_vio,
                               x = avg_vio_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

################################################################################
## Subset state analysis 
################################################################################

hb_vio_OK <- left_join(OK_bg, data_biscale_al)

summary(hb_vio_OK$minorpct)

# Map 1: Health based violations and % POC

map <- ggplot() +
  geom_sf(data = hb_vio_OK, mapping = aes(fill = bi_class), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  geom_sf(data = OK_cnty, fill = NA, color = "#969696") +
  theme(plot.background = element_blank()) 

breaks <- list(bi_x = c("0", "1", "2", "176"),
               bi_y = c("0", "0.21", "0.45", "1"))


legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% People of color →",
                    size = 50,
                    breaks = breaks,
                    arrows = FALSE,
                    base_family = "serif")


# Plot and legend

hb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.005, y = 0.15, width = 0.4, height = 0.4)

png(file = paste0(plot_path,"hb_POC_biv_OK_epic.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 17,
    bg = "transparent")

print(hb_minor)

dev.off()


################################################################################
## Map epic boundaries 
################################################################################

epic <- st_read("epic_boundaries/temm.gpkg") %>%
  st_transform(crs = 4326)

states <- tigris::states() %>%
  st_transform(crs = 4326)

OK_st <- states %>%
  filter(STUSPS == "OK") 

# Map 1: Health based violations and % POC

OK_epic <- epic %>%
  filter(state_code == "OK")

map <- ggplot() +
  geom_sf(data = OK_cnty, fill = NA, color = "#969696") +
  geom_sf(data = OK_st, fill = NA, color = "#636363") +
  geom_sf(data = OK_epic, fill = scales::alpha("red",0.7), color = "#525252") +
  coord_sf() +
  theme_void() 

png(file = paste0(plot_path,"OK_epic_2.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 17,
    bg = "transparent")

print(map)

dev.off()

################################################################################
## Map hm boundaries 
################################################################################

hm_boundary <- sf::st_read("Data/hall_murray_final/Final.shp") %>%
  st_transform(crs = 4326)

sdwa_pws <- read.csv("data/water quality/SDWA_PUB_WATER_SYSTEMS.csv")

OK_sys <- sdwa_pws %>%
  filter(STATE_CODE == "OK")

# Map 1: Health based violations and % POC

OK_hm <- hm_boundary %>%
  filter(PWSID %in% OK_sys$PWSID)

map <- ggplot() +
  geom_sf(data = OK_cnty, fill = NA, color = "#969696") +
  geom_sf(data = OK_st, fill = NA, color = "#636363") +
  geom_sf(data = OK_hm, fill = "#2ca25f", color = "#525252") +
  coord_sf() +
  theme_void() 

png(file = paste0(plot_path,"OK_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 17,
    bg = "transparent")

print(map)

dev.off()

