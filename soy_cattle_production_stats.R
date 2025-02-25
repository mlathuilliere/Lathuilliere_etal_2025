# Script to produce soy and cattle production statistics for:
# ... "International appropriation of Brazil's water through soy and beef supply chains"
#
# Author: Michael J. Lathuillière
# Contact michael.lathuilliere@sei.org with any questions

# Workstation set up ------------------------------------------------------

# Load packages
library(tidyverse)
library(sf)

# Load data ---------------------------------------------------------------

# define tmp file
# >>> Be sure to specify here the correct directory where the data are located
tmp <- # >>> Be sure to specify here the correct directory where the data are located

# Load the .rdata file
load(file= paste0(tmp, "soy_cattle_production.RData"))

# the above file loads the following data objects: 

## map information
### br - the map of Brazil (IBGE)
### mun - Brazilian municipalities, their IDs and boundaries (IBGE)
### br_regions - the 5 regions of Brazil (IBGE)
### br_basins - the macro basins of Brazil according (ANA)

## data
### soy - soy production and performance per municipality for 2015-2017
### cattle - beef production (as live weight) and performance per 
###          municipality for 2010-2017
### soy_bench - soy production benchmarks - not used here (see figures)
### cattle_bench - beef production benchmarks (as living herd in live weight)
###                - not used here (see figures)


# derive results for table 1  ----------------------------------------------

# soy (data also used for Figure 3, S8 and S9)

soy_br <- soy %>% 
  group_by(year) %>% 
  summarise(Mtonnes_sum = round(sum(tonnes, na.rm = T)*10^-6, 1),
            Mha_planted_sum = round(sum(planted_ha, na.rm = T)*10^-6, 1),    
            Mha_def_5y_tot_sum = round(sum(soy_def_5y_tot, na.rm = T)*10^-6, 2),
            Mtco2_g_5y_tot_sum = round(sum(soy_ghg_g_5y_tot, na.rm = T)*10^-6),
            km3_w_rock_sum = round(sum(soy_wf_rock*tonnes, na.rm = T)*10^-9),
            km3_gw_rock_sum = round(sum(soy_gwf_rock*tonnes, na.rm = T)*10^-9),
            km3_bw_rock_sum = round(sum(signif(soy_bwf_rock*tonnes, 4), na.rm = T)*10^-9, 2),
            Mha_irr_sum = round(sum(area_soy_pivot_mapb_ha, na.rm = T)*10^-6, 2)) %>% 
  ungroup() %>% 
  mutate(yield_mean = round(Mtonnes_sum*10^6/(Mha_planted_sum*10^6), 2),  # note that yield here is "planted" not "harvested"
         def_per_ktonne_mean = signif(Mha_def_5y_tot_sum/(Mtonnes_sum*10^-3), 2), 
         def_co2_per_tonne_mean = signif(Mtco2_g_5y_tot_sum/Mtonnes_sum, 2),
         wf_rock_mean = round(km3_w_rock_sum*10^9/(Mtonnes_sum*10^6)),
         gwf_rock_mean = round(km3_gw_rock_sum*10^9/(Mtonnes_sum*10^6)),
         bwf_rock_mean = round(km3_bw_rock_sum*10^9/(Mtonnes_sum*10^6)),
         pct_bw_rock = round(100*bwf_rock_mean/wf_rock_mean, 2))

View(soy_br)

# cattle

cattle_br <- cattle %>% 
  filter(year %in% 2015:2017) %>% 
  group_by(year) %>% 
  summarise(Mtonnes_lw = round(sum(total_beef_dairy_anualpec_lw_touros, na.rm = T)*10^-9, 1),
            Mha_pasture_sum = round(sum(pasture_ha_db*10^-6, na.rm = T)),    
            Mha_pasture_def_5y_tot_sum = round(sum(pasture_def_5y_tot, na.rm = T)*10^-6, 1),
            Mtco2_pasture_ghg_g_5y_tot_sum = round(sum(pasture_ghg_g_5y_tot, na.rm = T)*10^-6),
            Mtco2_animal_beef_dairy_anualpec_sum_h = round(sum(total_beef_dairy_anualpec_tot_co2eq_animal_h, na.rm = T)*10^-9),
            Mtco2_animal_beef_dairy_anualpec_sum_l = round(sum(total_beef_dairy_anualpec_tot_co2eq_animal_l, na.rm = T)*10^-9),
            km3_pasture_gw = round(sum(pasture_m3_gw*10^-9, na.rm = T)),
            km3_total_beef_dairy_anualpec_m3_zanetti_sum = round(sum(total_beef_dairy_anualpec_m3_zanetti*10^-9, na.rm = T), 1),
            e_imp_km3_y_sum = round(sum(e_imp_m3_y*10^-9, na.rm = T), 1),
            Mha_water_area_sum = round(sum(round(water_area_ha)*10^-6, na.rm = T), 2),
            cattle_stock_2017_mean = round(mean(cattle_stock_2017, na.rm = T), 2)) %>%   
  ungroup() %>% 
  mutate(
    def_per_ktonlw_mean = signif(Mha_pasture_def_5y_tot_sum/(Mtonnes_lw*10^-3), 3),
    def_per_ktonne_mean = round(Mtco2_pasture_ghg_g_5y_tot_sum/Mtonnes_lw, 1),
    cf_animal_l = round(Mtco2_animal_beef_dairy_anualpec_sum_l/Mtonnes_lw, 1),
    cf_animal_h = round(Mtco2_animal_beef_dairy_anualpec_sum_h/Mtonnes_lw, 1),
    gwf_mean = signif(km3_pasture_gw*10^9/(Mtonnes_lw*10^6), 3),
    bwf_animal_mean = round(km3_total_beef_dairy_anualpec_m3_zanetti_sum*10^9/(Mtonnes_lw*10^6)),
    bwf_reservoir_mean = round(e_imp_km3_y_sum*10^9/(Mtonnes_lw*10^6)))
     
View(cattle_br)
