################################################################################
# Calculating Drinking Water Quality across Demographic Groups  
# National Center for Environmental Economics
# Last edited: 6/28/2024
################################################################################

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  modelsummary, # regression table generation
  stringr, # string manipulation
  magrittr,
  tidycensus, #Census data
  MASS , #For regressions and modeling
  dplyr,
  readxl
)

################################################################################
##Set directories
################################################################################

#Tina
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## Prepare the datasets for the loop function 
################################################################################

################################################################################

# data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio)

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

################################################################################
## Creating data frames for each racial category
################################################################################

## Create a loop to get health based violations indicators

# Set up the function call 

all_risk <- data.frame()

pws_obs <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(pws_obs) <- c("obs", "indicator", "boundary")

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/HB_vio_',b ,'.rds'))
  
  lcr_vio <- readRDS(paste0('data/combined/lcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pfas_vio_',b ,'.rds')) %>%
    mutate(total_violations = concentration_avg)
  
  dbp_vio <- readRDS(paste0('data/combined/dbp_vio_',b ,'.rds')) %>%
    mutate(total_violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/tcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = detection_share)
  
  ars_vio <- readRDS(paste0('data/combined/ars_vio_',b ,'.rds')) %>%
    mutate(total_violations = ars_ug)
  
  nitrate_vio <- readRDS(paste0('data/combined/nitrate_vio_',b ,'.rds')) %>%
    mutate(total_violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  dem_risk = data.frame()
  
  my_df <- data.frame(race_cat = c("amer_ind", "asian", "black", "hisp", "pac_isl", "poc_risk", "lowinc"))

for (j in 1:length(data_list)) {
  
  water_data <- data_list[[j]]
  
  i = nth(indicator, j)
  
  rel_risk <- water_data %>%
    mutate(pop_served = as.numeric(pop_served)) %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
    mutate(asian_served = frac_asian*pop_served) %>% #asian served
    mutate(black_served = frac_black*pop_served) %>% #black population served
    mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
    mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
    mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
    mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
    mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
    mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
    mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(minor_risk = weighted.mean(total_violations, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight
  
  # Saving an intermediate file to construct an analysis panel later 
  write.csv(rel_risk, file = paste0("ej_service_areas/data/risk_by_pws/risk_", b, "_", i,".csv" ))
  
  pws_risk <- rel_risk[1, (ncol(rel_risk)-8):ncol(rel_risk)]
  
  pws_risk$indicator <- i
  
  dem_risk <- bind_rows(dem_risk, pws_risk) 
  
  
  # generate relative risk metrics 
  amer_ind = as.numeric(rel_risk[1, "amer_ind_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  asian = as.numeric(rel_risk[1, "asian_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  black = as.numeric(rel_risk[1, "black_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  hisp = as.numeric(rel_risk[1, "hispanic_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  pac_isl = as.numeric(rel_risk[1, "pac_isl_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  minor_risk = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  lowinc_risk = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
  
  
  risks <- data.frame(risk = c(amer_ind, asian, black, hisp, pac_isl, minor_risk, lowinc_risk))
  
  my_df <- cbind(my_df, risks)
  
  colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
  
  # Get observation count
  
  obs <- sum(!is.na(rel_risk$total_violations))
  
  pws_obs[nrow(pws_obs) + 1, ] = c(obs, i, b)
  
}
  
dem_risk$boundary <- b
  
write.csv(dem_risk, file = paste0("ej_service_areas/data/risk_by_pws/risk_", b, "_combined.csv"))

my_df$boundary <- b

write.csv(my_df, file = paste0("ej_service_areas/data/rel_risk/rel_risk_all_subpop_", b, ".csv"))

all_risk <- bind_rows(all_risk, my_df)  

}

write.csv(all_risk, file = "ej_service_areas/data/rel_risk/rel_risk_all_subpop.csv")

all_risk <- all_risk %>%
  group_by(race_cat)

all_risk <- read.csv("ej_service_areas/data/rel_risk/rel_risk_all_subpop.csv") 


###################################
# Combine risk by PWS 

boundary = c("county", "zc", "usgs", "epic", "hm")

# file = paste0("ej_service_areas/data/risk_by_pws/",b,"_risk_combined.csv")

comb_risk <- data.frame()

# read data

for (b in boundary) {
  dem_risk <- read.csv(file = paste0("ej_service_areas/data/risk_by_pws/risk_",b,"_combined.csv"), )
  
  comb_risk <- rbind(comb_risk, dem_risk)
  
}

var.names <- c(
  "Boundary" = "boundary",
  "American Indian" = "amer_ind_risk",
  "Asian" = "asian_risk",
  "Black" = "black_risk",
  "Hispanic" = "hispanic_risk",
  "Pacific Islander" = "pac_isl_risk",
  "Non-Hispanic White" = "nhw_risk",
  "POC" = "minor_risk",
  "Above 2X FPL" = "highinc_risk",
  "Below 2X FPL" = "lowinc_risk"
  
)

boundaries <- c(county = "County", zc = "Zip Code", usgs = "USGS", epic = "EPIC", hm = "EPA ORD")

comb_risk_2 <- comb_risk %>%
  relocate(boundary, .before =amer_ind_risk) %>%
  rename(var.names) %>%
  mutate(across(is.numeric, round, 2)) %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate'))) %>%
  # dplyr::select(c(1:2), sort(colnames(.))) %>%
  dplyr::select(c(2:11)) %>%
  mutate(Boundary = str_replace_all(Boundary, boundaries))
  
################################
### create table 

  
table <- kbl(comb_risk_2, booktabs = T, format = 'latex') %>%
  column_spec(1, width = '2cm') %>%
  column_spec(2:10, width = '1.25cm') %>%
  pack_rows("Health-based Violations (2015-2022)", 1, 5) %>%
  pack_rows("Lead Action Level Exceedences (1991-2021)", 6, 10) %>%
  pack_rows("PFAS Concentrations (2013-2023)", 11, 15) %>%
  pack_rows("TTHM HAA5 Concentrations (2006-2019) ", 16, 20) %>%
  pack_rows("Total Coliform Detection Share (2006-2019)", 21, 25) %>%
  pack_rows("Arsenic Concentration (2006-2019)", 26, 30) %>%
  pack_rows("Nitrate Concentration (2006-2019)", 31, 35)
  
table

################
#Table for EPIC, ORD, County

dem_cat <- c(
  "boundary" = "Boundary",
  "amer_ind"  = "American Indian",
  "asian" = "Asian",
  "black" =  "Black",
  "hisp" ="Hispanic",
  "pac_isl"= "Pacific Islander",
  'lowinc' = "Low Income"
)

specs <- c("hm", "epic", "county", "usgs", "zipcode")

all_risk <- read.csv("ej_service_areas/data/rel_risk/rel_risk_all_subpop.csv") 
# %>%  filter(boundary %in% specs)

all_risk_2 <- all_risk %>%
  pivot_longer(cols = ends_with('risk'), cols_vary = "slowest") %>%
  pivot_wider(id_cols = c(race_cat, name), values_from = value, names_from = boundary) %>%
  relocate(name, .before = race_cat) %>%
  rename('County' = county, 'EPIC' = epic , "EPA ORD" = hm, "USGS" = usgs, "Zip code" = zc) %>%
  mutate(race_cat = str_replace_all(race_cat, dem_cat)) %>%
  filter(race_cat != "poc_risk") %>%
  rename(" " = race_cat) %>%
  dplyr::select(2:7) %>%
  mutate_if(is.numeric, round, 2)


table <- kbl(all_risk_2, booktabs = T, format = 'latex') %>%
  column_spec(1, width = '5cm') %>%
  column_spec(2:6, width = '2cm') %>%
  pack_rows("Health-based Violations (2015-2022)", 1, 6) %>%
  pack_rows("Lead Action Level Exceedences (1991-2021)", 7, 12) %>%
  pack_rows("PFAS Concentrations (2013-2023)", 13, 18) %>%
  pack_rows("TTHM HAA5 Concentrations (2006-2019) ", 19, 24) %>%
  pack_rows("Total Coliform Detection Share (2006-2019)", 25, 30)%>%
  pack_rows("Arsenic Concentration (2006-2019)", 31, 36) %>%
  pack_rows("Nitrate Concentration (2006-2019)", 37, 42)

table

