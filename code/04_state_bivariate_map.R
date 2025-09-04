################################################################################
# Bivariate maps for drinking water indicators
# National Center for Environmental Economics
# Last edited: 8/8/30
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
  biscale,
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
  mapview
)


################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "ej_service_areas/output/state bivariate maps/"

# Source code for bi_scale manipulations

source("Code/biscale_fn_action_level.R")

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
## Load cbg data for all US
################################################################################

US_cbg_crop <- readRDS("data/census_geo/US_cbg_crop.rds")

US_st_crop <- readRDS("data/census_geo/US_st_crop.rds")

################################################################################
## DBP concentrations in NC
################################################################################

cbg_dbp_vio <- read_rds("data/combined/dbp_vio_hm_area_v2.rds")%>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = mean(combined_dbp)) %>%
  distinct(ID, .keep_all = TRUE) 

nc_counties <- tigris::counties("NC", cb = TRUE) 

nc_cbg <- US_cbg_crop %>%
  filter(STATEFP == "37") %>%
  st_transform(crs = st_crs(nc_counties))

nc_border <- US_st_crop %>%
  filter(STATEFP == "37")

dbp_vio_nc <- left_join(nc_cbg, cbg_dbp_vio) 

ggplot(nc_counties) +
  geom_sf() +
  geom_sf_text(aes(label = NAME))

counties_sub <- nc_counties %>%
  filter(NAME == "Forsyth" | NAME == "Guilford")


################################################################################
## Create bivariate map : DBP in NC
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(dbp_vio_nc$avg_dbp_cbg) #distribution

summary(dbp_vio_nc$minorpct) #distribution

# Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1418  0.3216  0.3848  0.5956  1.0000

#3rd quartile = 0.01, so we will use that as a limit for "frequent violators"

action_vector <- c(45, 75) #Modify for indicator-specific

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(dbp_vio_nc,
                               x = avg_dbp_cbg, y = minorpct,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = nc_border, fill = NA, color = scales::alpha( "#969696", alpha = 1)) +
  geom_sf(data = nc_counties, fill = NA, color = scales::alpha( "#969696", alpha = 0.3)) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme(bg_color = "#ffffff")


# 
# map +   coord_sf(
#   xlim = sf::st_bbox(counties_sub)[c(1,3)],
#   ylim = sf::st_bbox(counties_sub)[c(2,4)],
#   expand = TRUE
# )

breaks <- list(bi_x = c("0", "45", "75", "120"),
               bi_y = c("0", "0.15", "0.60", "1"))

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Combined DBP (mg/L) →",
                    ylab = "% people of color →",
                    size = 30,
                    arrows = FALSE,
                    breaks = breaks,
                    base_family = "serif")

# Plot and legend

map <- 
  map +
  geom_rect(
    xmin = sf::st_bbox(counties_sub)[c(1)],
    ymin = sf::st_bbox(counties_sub)[c(2)],
    xmax = sf::st_bbox(counties_sub)[c(3)],
    ymax = sf::st_bbox(counties_sub)[c(4)],
    fill = NA, 
    colour = "black",
    linewidth = 0.4
  )

dbp_minor <- ggdraw() +
  draw_plot(map, x= 0, y = -0.22) +
  draw_plot(
    {map +   
        coord_sf(
          xlim = sf::st_bbox(counties_sub)[c(1,3)],
          ylim = sf::st_bbox(counties_sub)[c(2,4)],
          expand = FALSE
        )  +
        theme(legend.position = "none")
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.5, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.58,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.4, 
    height = 0.4) +
  draw_text("Forsyth County, NC", 0.52, 0.6, size = 30,family = "serif", hjust = 0) +
  draw_plot(legend, x = 0.1, y = 0.63, width = 0.32, height = 0.32)

png(file = paste0(plot_path,"dbp_POC_biv_nc.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(dbp_minor)

dev.off()

rm(dbp_minor, nc_cbg, nc_border, nc_counties, dbp_vio_nc, cbg_dbp_vio)

################################################################################
## Create bivariate map : PFAS in New Jersey
################################################################################

cbg_pfas_vio <- read_rds("data/combined/pfas_vio_hm_area.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID))  %>%
  group_by(ID) %>%
  mutate(avg_conc_cbg = mean(concentration_avg))  %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

nj_counties <- tigris::counties("NJ", cb = TRUE)

nj_border <- US_st_crop %>%
  filter(STATEFP == "34")

nj_cbg <- US_cbg_crop %>%
  filter(STATEFP == "34") %>%
  st_transform(crs = st_crs(nj_counties))

pfas_vio_nj <- left_join(nj_cbg, cbg_pfas_vio)

# ggplot(nj_counties) +
#   geom_sf() +
#   geom_sf_text(aes(label = NAME))

counties_sub <- nj_counties %>%
  filter( NAME == "Bergen" | NAME == "Essex" | NAME == "Hudson")

st_bbox(counties_sub) 

# xmin      ymin      xmax      ymax 
# -74.63104  40.25112 -73.98474  40.90897 

################################################################################
## Create bivariate map : PFAS
################################################################################

summary(pfas_vio_nj$avg_conc_cbg) 
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   2.394  11.390  16.808  27.663 462.789    1506 

summary(pfas_vio_nj$minorpct) 


action_vector <- c(0.9,27) #Modify for indicator-specific

#Establish biscale comparison: pfas violations and % POC

data_biscale_al <- bi_class_al(pfas_vio_nj,
                               x = avg_conc_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: PFAS concentrations and POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = nj_border, fill = NA, color = scales::alpha( "#969696", alpha = 1)) +
  geom_sf(data = nj_counties, fill = NA, color = scales::alpha( "#969696", alpha = 0.3)) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

# map +   coord_sf(
#   xlim = sf::st_bbox(counties_sub)[c(1,3)],
#   ylim = sf::st_bbox(counties_sub)[c(2,4)],
#   expand = TRUE
# )

breaks <- list(bi_x = c("0", "1", "27", "460"),
               bi_y = c("0", "0.17", "0.72", "1"))

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "# PFAS →",
                    ylab = "% People of color →",
                    size = 30,
                    arrows = FALSE,
                    breaks = breaks,
                    base_family = "serif")

# Plot and legend

map <- 
  map +
  geom_rect(
    xmin = sf::st_bbox(counties_sub)[c(1)],
    ymin = sf::st_bbox(counties_sub)[c(2)],
    xmax = sf::st_bbox(counties_sub)[c(3)],
    ymax = sf::st_bbox(counties_sub)[c(4)],
    fill = NA, 
    colour = "black",
    linewidth = 0.4
  )

pfas_minor <- ggdraw() +
  draw_plot(map, x= -0.15, y = 0) +
  draw_plot(
    {map +   
        coord_sf(
          xlim = sf::st_bbox(counties_sub)[c(1,3)],
          ylim = sf::st_bbox(counties_sub)[c(2,4)],
          expand = FALSE
        )  +
        theme(legend.position = "none")
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.37, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.5,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.46, 
    height = 0.46) +
  draw_text("Bergen County, NJ", 0.51, 0.48, size = 30,family = "serif", hjust = 0) +
  draw_plot(legend, x = 0.45, y = 0.05, width = 0.32, height = 0.32)

png(file = paste0(plot_path,"pfas_minor_biv_nj.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pfas_minor)

dev.off()

rm(pfas_minor, nj_cbg, nj_border, nj_counties, pfas_vio_nj, cbg_pfas_vio)


################################################################################
## LCR violations in Ohio
################################################################################

cbg_lcr_vio <- readRDS("data/combined/lcr_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))%>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(pb_vio_cbg = mean(pb_vio_count)) %>% #average number of lead violations per CBG
  mutate(cu_vio_cbg = mean(cu_vio_count)) %>% #average number of copper violations per CBG
  mutate(avg_pb_cbg = mean(avg_pb_level)) %>% #average lead levels per CBG
  mutate(avg_cu_cbg = mean(avg_cu_level)) %>% #average copper levels per CBG
  distinct(ID, .keep_all = TRUE)

oh_counties <- tigris::counties("OH", cb = TRUE) 

oh_cbg <- US_cbg_crop %>%
  filter(STATEFP == "39") %>%
  st_transform(crs = st_crs(oh_counties))

oh_border <- US_st_crop %>%
  filter(STATEFP== "39")

lcr_vio_oh <- left_join(oh_cbg, cbg_lcr_vio) 

counties_sub <- oh_counties %>%
  filter(NAME == "Hamilton")

# ggplot(oh_counties)  +
#   geom_sf() +
#   geom_sf_text(aes(label = NAME))
# 
# franklin_vio <- lcr_vio_oh %>% subset(COUNTYFP == "049")
# 
# st_bbox(counties_sub) 

################################################################################
## Create bivariate map : Lead in OH
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_oh$avg_pb_cbg) #distribution

summary(lcr_vio_oh$pb_vio_cbg) #distribution

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    1.00    1.00    1.58    2.00    8.00    3341 

summary(lcr_vio_oh$minorpct) #distribution

#3rd quartile = 0.01, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9, 2) #Modify for indicator-specific

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(lcr_vio_oh,
                               x = pb_vio_cbg, y = minorpct,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = oh_counties, fill = NA, color = scales::alpha( "#969696", alpha = 0.3)) +
  geom_sf(data = oh_border, fill = NA, color = scales::alpha( "#969696", alpha = 1)) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  coord_sf() +
  bi_theme(bg_color = "#ffffff")

# 
# map +   coord_sf(
#   xlim = sf::st_bbox(counties_sub)[c(1,3)],
#   ylim = sf::st_bbox(counties_sub)[c(2,4)],
#   expand = TRUE
# )

breaks <- list(bi_x = c("0", "1", "2", "8"),
               bi_y = c("0", "0.04", "0.35", "1"))

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Number of ALEs →",
                    ylab = "% People of color →",
                    size = 30,
                    arrows = FALSE,
                    breaks = breaks,
                    base_family = "serif")

# Plot and legend

# pb_minor <- cowplot::ggdraw() +
#   draw_plot(map, x = 0, y = 0.05, width = 0.95, height = 0.95) +
#   draw_plot(legend, x = 0.70, y = 0.05, width = 0.3, height = 0.3)

map <- 
  map +
  geom_rect(
    xmin = sf::st_bbox(counties_sub)[c(1)],
    ymin = sf::st_bbox(counties_sub)[c(2)],
    xmax = sf::st_bbox(counties_sub)[c(3)],
    ymax = sf::st_bbox(counties_sub)[c(4)],
    fill = NA, 
    colour = "black",
    linewidth = 0.4
  )

pb_oh_minor <- ggdraw() +
  draw_plot(map, x= 0.18, y = 0) +
  draw_plot(
    {map +   
        coord_sf(
          xlim = sf::st_bbox(counties_sub)[c(1,3)],
          ylim = sf::st_bbox(counties_sub)[c(2,4)],
          expand = FALSE
        )  +
        theme(legend.position = "none")
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.0, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.07,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.46, 
    height = 0.46) +
  draw_text("Hamilton County, OH", 0.05, 0.06, size = 30,family = "serif", hjust = 0) +
  draw_plot(legend, x = 0.0, y = 0.56, width = 0.32, height = 0.32) 


png(file = paste0(plot_path,"pb_ale_POC_biv_oh.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_oh_minor)

dev.off()

rm(lcr_minor, oh_cbg, oh_border, oh_counties, lcr_vio_oh, cbg_lcr_vio)


# This one I like 

