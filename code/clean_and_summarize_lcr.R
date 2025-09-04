## Clean and summarize drinking water data

# Last edited: 2/27/2025

## 01: Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf, # vector data operations
  maps, # to get county boundary data
  dplyr, # data wrangling
  lubridate, # Date object handling
  ggplot,
  tidycensus
)


## Lead and Copper rule raw data

## Load lcr data

## Downloaded from https://echo.epa.gov/tools/data-downloads#downloads

lcr_samp <- read.csv("Data/SDWA raw/SDWA_lcr_SAMPLES.csv")

length(unique(lcr_samp$PWSID))

## find the samples exceeding 0.015 mg/l lead and 1.3mg/L for copper, 
# Group by violation type and by PWSID

# Create different variables for different violation types

## 2 continuous: average sample for each pwsid across the period for lead and for copper 

## 2 discrete : number of lead and number of copper exceedances at each pwsid 

lcr_vio <- lcr_samp %>%
  mutate(cu_vio = case_when(
    CONTAMINANT_CODE == "PB90" ~ NA, 
    SAMPLE_MEASURE >= 1.3 & CONTAMINANT_CODE == "CU90"  ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(pb_vio = case_when(
    SAMPLE_MEASURE >= 0.015 & CONTAMINANT_CODE == "PB90" ~ TRUE, 
    CONTAMINANT_CODE == "CU90"  ~ NA,
    TRUE ~ FALSE
  ))  %>%
  mutate(SAMPLE_MEASURE = as.numeric(SAMPLE_MEASURE)) %>%
  mutate(SAMPLE_MEASURE = case_when(SAMPLE_MEASURE < 0 ~ 0,
                                    TRUE ~ SAMPLE_MEASURE))

lcr_vio_2 <- lcr_vio %>%
  group_by(CONTAMINANT_CODE, PWSID) %>%
  mutate(cu_vio_count = sum(cu_vio, na.rm = TRUE)) %>%
  mutate(pb_vio_count = sum(pb_vio, na.rm = TRUE)) %>%
  mutate(avg_cu_level = ifelse(CONTAMINANT_CODE == "CU90", mean(SAMPLE_MEASURE), NA)) %>%
  mutate(avg_pb_level = ifelse(CONTAMINANT_CODE == "PB90", mean(SAMPLE_MEASURE), NA)) %>%
  mutate(max_cu_level = ifelse(CONTAMINANT_CODE == "CU90", max(SAMPLE_MEASURE), NA)) %>%
  mutate(max_pb_level = ifelse(CONTAMINANT_CODE == "PB90", max(SAMPLE_MEASURE), NA)) %>%
  ungroup(CONTAMINANT_CODE) %>%
  fill(c('avg_pb_level', 'avg_cu_level', 'max_cu_level', 'max_pb_level'), .direction = "downup") %>%
  mutate(cu_vio_count = max(cu_vio_count)) %>%
  mutate(pb_vio_count = max(pb_vio_count)) %>%
  ungroup()

#checking that pb and cu levels are filled in correctly
## not run
# y <- filter(lcr_vio, PWSID == "IL3049031")

## Clean data, keep only the total number of violations per PWSID and the maximum contamination in each

summary(lcr_vio$avg_cu_level)

summary(lcr_vio$avg_pb_level)

lcr_vio_pwsid <- lcr_vio_2 %>%
  distinct(PWSID, .keep_all = TRUE) %>%
  dplyr::select(CONTAMINANT_CODE, PWSID, cu_vio_count, pb_vio_count, 
                avg_cu_level, avg_pb_level, max_cu_level, max_pb_level) %>%
  filter(is.na(avg_pb_level) | avg_pb_level < 7000) %>% # filter out major outliers but keep NAs
  filter(is.na(avg_cu_level) | avg_cu_level < 15000)

summary(lcr_vio_pwsid$avg_cu_level)

summary(lcr_vio_pwsid$avg_pb_level)

## Ohio has really high occurrence levels; take a peek

ohio_vio <- lcr_vio_pwsid %>%
  mutate(st_abb = substr(PWSID, 1,2)) %>%
  filter(st_abb == "OH")

summary(ohio_vio$SAMPLE_MEASURE)


## Save as RDS and CSV for use
write_rds(lcr_vio_pwsid, "Data/lcr_violations.rds")

write.csv(lcr_vio_pwsid, "Data/lcr_violations.csv")


###
# Generate CBG boundaries for lead violations at the PWSID level
###

# Load lead violations

lcr_vio <- readRDS("Data/lcr_violations.rds") %>%
  clean_names


## Two different boundary types
boundary <- c("epic", "epa_ord")


for (i in boundary) {
  
  ## Boundary datasets from 00_get_dem_boundaries.rds
  pwsid_dem <- read.csv(paste0("Data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, lcr_vio, by = "pwsid", relationship = "many-to-many") %>%
    mutate(pb_vio_count = coalesce(pb_vio_count, 0),
           cu_vio_count = coalesce(cu_vio_count, 0),
           avg_cu_level = coalesce(avg_cu_level, 0),
           avg_pb_level = coalesce(avg_pb_level, 0),
           max_cu_level = coalesce(max_cu_level, 0),
           max_pb_level = coalesce(max_pb_level, 0))
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("Data/combined/area/lcr_vio_", i, "_area.rds"))
  
}

rm(lcr_vio)

##### 
# Generate violation maps
#####

## Path to save plots

plot_path <- "Plots/country/"

## Source code : create flexible break points for action levels

source("Code/biscale_fn_action_level.R")

custom_pal3_4 <- c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#9c9ecd",
  "3-1" = "#646ac7", # high x, low y
  "1-2" = "#c3c594",
  "2-2" = "#909390", # medium x, medium y
  "3-2" = "#5c638c",
  "1-3" = "#b1b552", # low x, high y
  "2-3" = "#83884f",
  "3-3" = "#545b4d" # high x, high y
)


## Create national spatial boundary basis for maps 

US_cbg <- tigris::block_groups(state = NULL, county = NULL, cb = TRUE) 

st_crs(US_cbg)

US_cbg_50 <- US_cbg %>%
  rename(ID = GEOID) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  dplyr::filter(STATEFP <= 56, STATEFP != 43) %>% #remove Puerto Rico and outer territories
  tigris::shift_geometry() %>%
  st_transform(crs = 5070) # this CRS gives the neatest projections

summary(US_cbg_50$STATEFP) #quick check that we have the states we want

US_50 <- tigris::states(cb = TRUE) %>%
  rename(ID = GEOID) %>%
  mutate(STATEFP = as.numeric(STATEFP))  %>%
  dplyr::filter(STATEFP <= 56, STATEFP != 43) %>% #remove Puerto Rico and outer territories
  tigris::shift_geometry() %>%
  st_transform(crs = 5070) # this CRS gives the neatest projections

US_48 <- tigris::states(cb = TRUE) %>%
  rename(ID = GEOID) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  dplyr::filter(STATEFP <= 56, STATEFP != 43, STATEFP != 2, STATEFP != 15) %>% #remove Puerto Rico and outer territories
  st_transform(crs = 5070)

#Get the bounding box for the continental US to guide the cropping

st_bbox(US_48) 

# xmin       ymin       xmax       ymax 
# -2356113.7   269573.6  2258200.2  3172567.9 

bbox <- st_bbox(c(xmin = -2356113.7 , ymin =  59573.6, 
                  xmax = 2258200.2, ymax = 3172567.9 ), 
                crs = st_crs(5070))

US_cbg_crop <- st_crop(US_cbg_50, bbox)

US_st_crop <- st_crop(US_50, bbox)

rm(US_48, US_cbg, US_cbg_50, US_50)

#####
## Create bivariate map : Lead levels
#####


cbg_lcr_vio <- read_rds("Data/combined/area/lcr_vio_ORD_area.rds") #load lcr data

## PWSID and CBG don't overlap perfectly so
## need to find average value based on overlap of 
## water systems at the CBG level

cbg_lcr_vio <- cbg_lcr_vio %>%
  group_by(ID) %>% ## PWSID
  mutate(pb_vio_cbg = mean(pb_vio_count)) %>% #average number of lead violations per CBG
  mutate(cu_vio_cbg = mean(cu_vio_count)) %>% #average number of copper violations per CBG
  mutate(avg_pb_cbg = mean(avg_pb_level)) %>% #average lead levels per CBG
  mutate(avg_cu_cbg = mean(avg_cu_level)) %>% #average copper levels per CBG
  distinct(ID, .keep_all = TRUE)

lcr_vio_all <- left_join(US_cbg_crop, cbg_lcr_vio) # Combine spatial polygon to DW values

### Map it

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_all$avg_pb_level) #distribution

#3rd quartile = 0.01, so we will use that as a limit for "frequent violators"

action_vector <- c(0.005, 0.015) #Modify for indicator-specific values: low detection value and violation

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = avg_pb_cbg, y = MINORPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme(bg_color = "#ffffff")

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "Lead Levels →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)


png(file = paste0(plot_path,"pb_lvl_POC_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_minor)

dev.off()

## Done! 

