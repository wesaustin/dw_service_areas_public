

################################################################################
# Regressions for indicators
# National Center for Environmental Economics
# Latest update: 1/23/2024
################################################################################

################################################################################
## Load packages: 
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  stargazer, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  stringr, # string manipulation
  magrittr,
  janitor,
  MASS , #for regressions
  stargazer, 
  pscl,
  psych
)



## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")


################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

# Model Variables: 

# Environment: Ozone, PM2.5, Superfund Site, Hazardous Waste, Underground Waste Storage
# Wasterwater distribution, Toxic Release site, Lead

# Demographic: % Black, % Hispanic, % Pacific Islander, % American Indian, % Asian, 
# % Low Income, System Size

################################################################################
## Load system data on water source 
################################################################################

sys_code <- read.csv("data/demographics/SDWA_PUB_WATER_SYSTEMS.csv") %>%
  dplyr::select(PWSID, GW_SW_CODE, PRIMACY_TYPE) %>%
  clean_names %>%
  mutate(source_gw = ifelse(gw_sw_code == "GW", 1, 0))

################################################################################
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("data/combined/HB_vio_hm.rds") %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))


# #Negative binomial regression
# summary(hb_dem_nb <- glm.nb(total_violations ~  minorpct +  lowinc + primacy_type + SystemSize + source_gw, 
#                             data = HB_vio))

# summarize outcome variable 

sumstats <- psych::describe(HB_vio$total_violations)
variance <- var(HB_vio$total_violations , na.rm = TRUE)

# OLS
summary(hb_dem_ols <- glm(total_violations ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          data = HB_vio))

# poisson model 
summary(hb_dem_poi <- glm(total_violations ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          family = "poisson",
                          data = HB_vio))

# negative binomial model 
summary(hb_dem_nb <- glm.nb(total_violations ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                               lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw ,
                      data = HB_vio))

# zero-inflated poisson model 
summary(hb_dem_zip <- zeroinfl(total_violations ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                               lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw  | primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw  ,
                             data = HB_vio))

# frac_hisp + 

# The above does not converge, likely due to multicolinearity... When I take out hisp the code works
# WA: it converged for me without issue 

summary(hb_env_ols <- glm(total_violations ~ pre1960pct + ozone + pm25 + ptsdf + 
                            pwdis + pnpl , family = 'poisson',
                          data = HB_vio))

# Clear memory
rm(HB_vio)

################################################################################
## lcr violations 
################################################################################

lcr_vio <- read_rds("data/combined/lcr_vio_hm.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

## Lead violations (count)

sumstats <- psych::describe(lcr_vio$pb_vio_count)
variance <- var(lcr_vio$pb_vio_count , na.rm = TRUE)

# OLS
summary(lcr_dem_ols <- glm(pb_vio_count ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          data = lcr_vio))

# poisson model 
summary(lcr_dem_poi <- glm(pb_vio_count ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          family = "poisson",
                          data = lcr_vio))

# negative binomial model 
summary(lcr_dem_nb <- glm.nb(pb_vio_count ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw ,
                            data = lcr_vio))

# zero-inflated poisson model 
summary(lcr_dem_zip <- zeroinfl(pb_vio_count ~  frac_amerind +  frac_asian + frac_black + frac_hisp + frac_pacisl + 
                                 lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw  | primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + st_abb ,
                               data = lcr_vio))



summary(lcr_env_ols <- glm(pb_vio_count ~ pre1960pct + ozone + pm25 + ptsdf + 
                             pwdis + pnpl , family = 'poisson',
                           data = lcr_vio))

## Lead levels

# summary(lcr_dem_lm <- glm(avg_pb_level ~ frac_black + frac_hisp + frac_pacisl + 
#                              frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw,
#                           data = lcr_vio))
# 
# summary(lcr_env_lm <- glm(avg_pb_level ~ pre1960pct + ozone + pm25 + ptsdf + 
#                             pwdis + pnpl + source_gw,
#                           data = lcr_vio))

rm(lcr_vio)


################################################################################
## PFAS detections
################################################################################

pfas_vio <- read_rds("data/combined/pfas_vio_hm.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

# summary(pfas_dem_plm <- glm(detection ~ frac_black + frac_hisp + frac_pacisl + 
#                          frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + st_abb, 
#                        family = binomial(link="probit"), data = pfas_vio))

summary(pfas_dem_ols <- glm(concentration_avg ~ frac_amerind + frac_asian + frac_black + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                            family = "gaussian",
                            data = pfas_vio))

summary(pfas_env_ols <- glm(concentration_avg ~ pre1960pct + ozone + pm25 + ptsdf + 
                              pwdis + pnpl , 
                            family = "gaussian", data = pfas_vio))

# Clear memory
rm(pfas_vio)


################################################################################
## DBP concentrations
################################################################################

dbp_vio <- read_rds("data/combined/dbp_vio_hm.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(dbp_dem_lm <- glm(combined_dbp ~ frac_amerind + frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          data = dbp_vio))

summary(dbp_env_lm <- glm(combined_dbp ~ pre1960pct + ozone + pm25 + ptsdf + 
                            pwdis + pnpl ,
                          data = dbp_vio))


# Clear memory
rm(dbp_vio)


################################################################################
## TCR detection rate 
################################################################################

tcr_vio <- read_rds("data/combined/tcr_vio_hm.rds") %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(tcr_dem_lm <- glm(detection_share ~ frac_amerind + frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          # family = binomial(link = "logit"),
                          data = tcr_vio))

summary(tcr_env_lm <- glm(detection_share ~ pre1960pct + ozone + pm25 + ptsdf + 
                            pwdis + pnpl , 
                          #    family = binomial(link = "logit"),
                          data = tcr_vio))

rm(tcr_vio)


################################################################################
## Arsenic concentrations
################################################################################

ars_vio <- read_rds("data/combined/ars_vio_hm.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

# re-express these in ppb 
ars_vio <- ars_vio %>%
  mutate(arsenic = arsenic * 1000 )

summary(ars_dem_lm <- glm(arsenic ~ frac_amerind + frac_asian + frac_black + frac_hisp + frac_pacisl + 
                            lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw , 
                          data = ars_vio))

summary(ars_env_lm <- glm(arsenic ~ pre1960pct + ozone + pm25 + ptsdf + 
                            pwdis + pnpl ,
                          data = ars_vio))


# Clear memory
rm(ars_vio)

################################################################################
## Nitrate concentrations
################################################################################

nitrate_vio <- read_rds("data/combined/nitrate_vio_hm.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(nitrate_dem_lm <- glm(nitrate ~ frac_amerind + frac_asian + frac_black + frac_hisp + frac_pacisl + 
                                lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw ,
                              data = nitrate_vio))

summary(nitrate_env_lm <- glm(nitrate ~ pre1960pct + ozone + pm25 + ptsdf + 
                                pwdis + pnpl ,
                              data = nitrate_vio))


# Clear memory
rm(nitrate_vio)

################################################################################
## Regression tables
################################################################################

#Dem regressions

#checking that table output looks the same as the stargazer output 
# summary(dbp_dem_lm)
# summary(ars_dem_lm)
# summary(nitrate_dem_lm)

# Noting there was an error message when trying to add the arsenic and nitrate regression 
# results. This was an issue with the Stargazer package when using r versions > 4.2, so there is som e
# new manual code above that allows this to run.
# The error was: "Error in if (is.na(s)) { : the condition has length > 1"

# for some reason the coefficients for Black and Asian were reversed.

stargazer(hb_dem_ols, lcr_dem_ols, pfas_dem_ols, dbp_dem_lm, tcr_dem_lm, ars_dem_lm, nitrate_dem_lm, 
          title = "Demographic Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR", "ARS", "NITR"),
          covariate.labels = c("\\% American Indian", "\\% Asian", "\\% Black", "\\% Hispanic", "\\% Pacific Islander",  
                               "\\% Low income^{+}", "Tribal System", "Large system^{++}", "Small system", "Very Large system", "Very small system","Groundwater"),
          omit = 'st_abb', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))

#Env regressions

stargazer(hb_env_ols, lcr_env_ols, pfas_env_ols, dbp_env_lm, tcr_env_lm, ars_env_lm, nitrate_env_lm, 
          title = "Environmental Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR", "ARS", "NITR"),
          covariate.labels = c("Lead Paint", "Ozone", "PM_{2.5}", "Toxic Release Facility", "
                         Wastewater discharge", "Superfund Site"),
          omit = 'st_abb', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))




