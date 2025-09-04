## Generate Generalized Lorenz curves
# Script adapted from code developed by Dr. Wes Austin of the US EPA

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  dplyr, # data wrangling
  ggplot2,
  janitor
)

################################################################################
##Set directories
################################################################################

#Tina
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

#WA
# my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

#############################
# Health based-vio lorenz

hb_vio <- read.csv('data/risk_by_pws/risk_hm_hb.csv') %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)
  

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- hb_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "Health-based violations") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/hb_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- hb_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "Health-based violations") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/hb_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()


#############################
# lead vio lorenz

lcr_vio <- read.csv('data/risk_by_pws/risk_hm_lcr.csv') %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- lcr_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "Lead ALEs") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/lcr_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- lcr_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "Lead ALEs") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/lcr_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

#############################
# PFAs vio lorenz

pfas_vio <- read.csv('data/risk_by_pws/risk_hm_pfas.csv') %>%
  filter(!is.na(concentration_avg)) %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- pfas_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "PFAS concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/pfas_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- pfas_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "PFAS concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/pfas_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

#############################
# dbp vio lorenz

dbp_vio <- read.csv('data/risk_by_pws/risk_hm_dbp.csv') %>%
  filter(!is.na(combined_dbp)) %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- dbp_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "DBP concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/dbp_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- dbp_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "DBP concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/dbp_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()


#############################
# tcr vio lorenz

tcr_vio <- read.csv('data/risk_by_pws/risk_hm_tcr.csv') %>%
  filter(!is.na(detection_share)) %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- tcr_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "Total coliform") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/tcr_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- tcr_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "Total coliform") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/tcr_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()


#############################
# ars vio lorenz

ars_vio <- read.csv('data/risk_by_pws/risk_hm_ars.csv') %>%
  filter(!is.na(arsenic)) %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- ars_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "Arsenic concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/ars_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- ars_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "Arsenic concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/ars_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

#############################
# nitrate vio lorenz

nitrate_vio <- read.csv('data/risk_by_pws/risk_hm_nitrate.csv') %>%
  filter(!is.na(nitrate)) %>%
  mutate(vio_weighted = total_violations*pop_served,
         minorpct = minorpct*pop_served,
         lowinc = lowinc*pop_served)

# Rank (i.e., sort) census divisions by poverty share (lowincpct), 
# and then compute the cumulative burden of PM 2.5 over the poverty share ranking  

lorenz_data <- nitrate_vio %>%
  arrange(desc(lowinc)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_poverty = (cumsum(lowinc) / sum(lowinc))*100,  # poverty share ranking
         # cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_poverty, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in poverty share", 
       y = "Nitrate concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/nitrate_lowinc_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

# Percent people of color

lorenz_data <- nitrate_vio %>%
  arrange(desc(minorpct)) %>%  # Arrange CBGs by poverty share
  mutate(cdf_minor = (cumsum(minorpct) / sum(minorpct))*100,
         cdf_vio = (cumsum(vio_weighted) / sum(vio_weighted))*100)  # cumulative violations exposure by pov share ranking 

# Plot the Lorenz curve using ggplot
lorenz_plot <- ggplot(lorenz_data, aes(x = cdf_minor, y = cdf_vio)) +
  geom_line(color = "#3B9AB2" , linewidth = 1.2) +   # Lorenz curve line
  geom_point(color = "#78B7C5", size = 2) +     # Points for each CBG (optional for clarity)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#F21A00" ) +  # Line of equality
  labs(x = "Percentile ranking in people of color share", 
       y = "Nitrate concentration") +
  theme_minimal(base_size = 18) +
  ylim(0, 100) +                               
  coord_fixed()  

png(file = 'ej_service_areas/output/lorenz curves/nitrate_minor_lorenz_wt.png', 
    width = 500, height = 700, units = "px", pointsize = 12,
    bg = "white")

print(lorenz_plot)

dev.off()

