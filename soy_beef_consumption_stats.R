# Script to produce soy and beef statistics for:
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
tmp <- # >>> Be sure to specify here the correct directory where the data are located

# Load the .rdata file
load(file= paste0(tmp, "soy_beef_consumption.RData"))

# the above file loads the following data objects: 

## map information
### br - the map of Brazil (IBGE)
### mun - Brazilian municipalities, their ids and boundaries (IBGE)
### basin_map - the macro basins of Brazil according the Brazilian Water
###             Agency (ANA)

## data
### trase_soy - soy exports and domestic consumption for 2015-2017 
###             (trase data v.2.6.1) and municipality-level indicators
### trase_beef - beef exports (as carcass weight) for 2015-2017 
###              (trase data v.2.2.0) and municipal-level indicators 
###              (note that trase data does not include domestic consumption 
###              of beef)
### soy - soy production and performance per municipality for 2015-2017
### beef - beef production (as carcass weight) and performance per municipality 
###        for 2015-2017
### ws_labels - water scarcity status as per ANA
### soy_bench - soy production benchmarks
### beef_bench - beef production benchmarks (as product in carcass weight)


# derive total water use for EU and China ---------------------------------

## add product to each trade dataset
trase_soy <- trase_soy %>% 
  mutate(product = "Soy")

trase_beef <- trase_beef %>% 
  mutate(product = "Beef")

## derive total water use per product and import country

summary_water_country_product <- rbind(
  
  trase_soy %>%
    mutate(volume_irr = ifelse(soy_bwf_rock == 0, 0, volume)) %>% 
    group_by(year, economic_bloc, product) %>%
    summarise(vol_tot = sum(volume, na.rm = T), 
              vol_tot_irr = sum(volume_irr, na.rm = T),
              km3_gw = round(sum(m3_gw_rock, na.rm = T)*10^-9, 1),
              km3_bw = round(sum(m3_bw_rock, na.rm = T)*10^-9, 2),
              ha_def_tot = round(sum(ha_def, na.rm = T))) %>% 
    ungroup() %>%
    mutate(km3_tot = round(km3_gw + km3_bw, 1)) %>% 
    filter(economic_bloc %in% c("CHINA", "EU")),
  
  trase_beef %>% 
    mutate(volume_irr = 0) %>% 
    group_by(year, economic_bloc, product) %>% 
    summarise(vol_tot = sum(volume, na.rm = T),
              vol_tot_irr = sum(volume_irr),
              km3_gw = round(sum(pasture_gw_m3, na.rm = T)*10^-9),
              km3_bw = round(sum(tot_m3_zanetti, na.rm = T)*10^-9, 2),
              ha_def_tot = round(sum(ha_def, na.rm = T))) %>% 
    ungroup() %>% 
    mutate(km3_tot = round(km3_gw + km3_bw, 1)) %>% 
    filter(economic_bloc %in% c("CHINA", "EU"))) %>% 
  
  select(year:km3_bw, km3_tot, ha_def_tot)


View(summary_water_country_product)

## derive total water use per import country (combining products)

summary_water_country <- summary_water_country_product %>% 
  group_by(year, economic_bloc) %>% 
  summarise(km3_gw = round(sum(km3_gw)),
            km3_bw = round(sum(km3_bw), 2),
            km3_tot = round(sum(km3_tot))) %>% 
  ungroup()

View(summary_water_country)

# derive results for Figure 3 ----------------------------------------------

## obtain total water use for the commodities, useful information for figure 3

### soy 

summary_water_country_soy <- trase_soy %>%
  group_by(year, economic_bloc, product) %>%
  summarise(Mt_volume = round(sum(volume, na.rm = T)*10^-6, 1),
            km3_gw = round(sum(m3_gw_rock, na.rm = T)*10^-9, 1),
            km3_bw = round(sum(m3_bw_rock, na.rm = T)*10^-9, 2),
            Mha_def = round(sum(ha_def, na.rm = T)*10^-6, 2),
            Mtco2_g_def = round(sum(tco2_g_def, na.rm = T)*10^-6)) %>% 
  ungroup() %>%
  mutate(km3_tot = round(km3_gw + km3_bw, 1)) %>% 
  filter(economic_bloc %in% c("CHINA", "EU", "BRAZIL")) %>% 
  arrange(desc(product), desc(year), economic_bloc) %>% 
  select(year:km3_bw, km3_tot, Mha_def, Mtco2_g_def)
 

### beef

summary_water_country_beef <- trase_beef %>% 
  group_by(year, economic_bloc, product) %>%
  summarise(Mt_volume = round(sum(volume, na.rm = T)*10^-6, 3),
            km3_gw = round(sum(pasture_gw_m3, na.rm = T)*10^-9),
            km3_bw = round(sum(tot_m3_zanetti, na.rm = T)*10^-9, 2),
            Mha_def = round(sum(ha_def, na.rm = T)*10^-6, 3),
            Mtco2_g_def = round(sum(tco2_def, na.rm = T)*10^-6, 1),
            Mtco2_l = round(sum(tco2_l, na.rm = T)*10^-6, 1),
            Mtco2_h = round(sum(tco2_h, na.rm = T)*10^-6, 1)) %>% 
  ungroup() %>%
  mutate(km3_tot = round(km3_gw + km3_bw, 1),
         Mtco2_mean = (Mtco2_l + Mtco2_h)/2,
         Mtco2_tot = round(Mtco2_g_def + Mtco2_mean)) %>% 
  filter(economic_bloc %in% c("CHINA", "EU", "BRAZIL")) %>% 
  arrange(desc(product), desc(year), economic_bloc) %>% 
  select(year:km3_bw, km3_tot, Mha_def, Mtco2_g_def, Mtco2_mean, Mtco2_tot)
  

View(summary_water_country_soy)
View(summary_water_country_beef)

### derive the values for Brazilian beef consumption

# unlike soy, the trase data does not provide domestic beef consumption, 
# we therefore derive it at the country-aggregated level using the difference 
# between production and exports

## Step 1: provide volumes for which a municipality of origin could not be 
## determined ("Unknown" flows)
## these flows are assumed to have no resource use or impacts/emissions

trase_beef_unk_vol <- tibble(
  year = c("2015", "2016", "2017"),
  Mtcw_trade_unk = c(140848*10^-6, 137313*10^-6, 142190*10^-6))

## Step 2: Derive exports from each state 

beef_trase_countries <-  trase_beef %>%              
  group_by(year, two_digit_code) %>%
  summarise(cw_trade = sum(volume, na.rm = T),
            ha_def_trade = sum(ha_def, na.rm = T),
            blue_m3_trade = sum(tot_m3_zanetti, na.rm = T),
            green_m3_trade = sum(pasture_gw_m3, na.rm = T),
            tco2_h_trade = sum(tco2_h, na.rm = T),
            tco2_l_trade = sum(tco2_l, na.rm = T),
            tco2_def_trade = sum(tco2_def, na.rm = T)) %>%
  ungroup()

## now derive the domestic consumption per state as remaining from exported beef

beef_br <- beef %>%
  group_by(year, two_digit_code) %>%
  
  # obtain flows from all of production
  summarise(cw_tot = sum(cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            ha_def_tot = sum(def_per_toncw_beef_dairy_anualpec*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            blue_m3_tot = sum(tot_m3_per_toncw_beef_dairy_anualpec_zanetti*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            green_m3_tot = sum(pasture_m3_gw_per_toncw_beef_dairy_anualpec*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            tco2_tot_h = sum(tco2eq_per_toncw_beef_dairy_anualpec_animal_h*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            tco2_tot_l = sum(tco2eq_per_toncw_beef_dairy_anualpec_animal_l*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T),
            tco2_def_tot = sum(tco2eq_per_toncw_beef_dairy_anualpec_def*cw_production_beef_dairy_anualpec_no_touros_ton, na.rm = T)) %>%
  ungroup() %>%
  left_join(beef_trase_countries, by = c("year", "two_digit_code")) %>%
  mutate(cw_trade = replace_na(cw_trade, 0),
         ha_def_trade = replace_na(ha_def_trade, 0),
         blue_m3_trade = replace_na(blue_m3_trade, 0),
         green_m3_trade = replace_na(green_m3_trade, 0),
         tco2_h_trade = replace_na(tco2_h_trade, 0),
         tco2_l_trade = replace_na(tco2_l_trade, 0),
         tco2_def_trade = replace_na(tco2_def_trade, 0),
         cw_br = cw_tot - cw_trade,
         ha_def_br = ha_def_tot - ha_def_trade,
         blue_m3_br = blue_m3_tot - blue_m3_trade, 
         green_m3_br = green_m3_tot - green_m3_trade,
         tco2_h_br = tco2_tot_h - tco2_h_trade,
         tco2_l_br = tco2_tot_l - tco2_l_trade,
         tco2_def_br = tco2_def_tot - tco2_def_trade,
         economic_bloc = "BRAZIL") %>%
  select(year, two_digit_code, cw_tot, cw_br, ha_def_br, blue_m3_br, green_m3_br, tco2_h_br, tco2_l_br, tco2_def_br, economic_bloc)

## provide total values for Brazilian domestic beef consumption 
## make sure to remove additional trade flows for which the municipality of 
## origin could not be derived (called "Unknown")

beef_br_tot <- beef_br %>% 
  group_by(year) %>% 
  summarise(Mtcw_br = round(sum(cw_br, na.rm = T)*10^-6, 2), 
            Mha_def_br = round(sum(ha_def_br, na.rm = T)*10^-6, 2),
            km3_bw_br = round(sum(blue_m3_br, na.rm = T)*10^-9), 
            km3_gw_br = round(sum(green_m3_br, na.rm = T)*10^-9),
            Mtco2_g_def_br = round(sum(tco2_def_br, na.rm = T)*10^-6),
            Mtco2_h_br = round(sum(tco2_h_br, na.rm = T)*10^-6),
            Mtco2_l_br = round(sum(tco2_l_br, na.rm = T)*10^-6),
            ) %>% 
  ungroup() %>% 
  mutate(km3_tot_br = round(km3_gw_br + km3_bw_br, 1),
         Mtco2_mean_br = round((Mtco2_l_br + Mtco2_h_br)/2),
         Mtco2_tot_br = round(Mtco2_g_def_br + Mtco2_mean_br)) %>%
  left_join(trase_beef_unk_vol) %>% 
  mutate(Mtcw_br_corr = round(Mtcw_br - Mtcw_trade_unk, 2)) %>%       
  select(-Mtcw_br, -Mtcw_trade_unk) %>% 
  rename(Mtcw_br = Mtcw_br_corr) %>% 
  select(year, Mtcw_br, km3_gw_br, km3_bw_br, km3_tot_br, Mha_def_br:Mtco2_tot_br)

View(beef_br_tot)

# macro basin analysis ----------------------------------------------------

## derive volume of **green and blue water** for each macro basin and ANA
## water scarcity

summary_water_country_macro <- rbind(
  
  trase_soy %>%
    group_by(year, economic_bloc, macro_basin) %>%
    summarise(km3_gw = round(sum(m3_gw_rock, na.rm = T)*10^-9, 1),
              km3_bw = round(sum(m3_bw_rock, na.rm = T)*10^-9, 2)) %>% 
    ungroup() %>%
    mutate(km3_tot_macro = round(km3_gw + km3_bw, 1)) %>% 
    filter(economic_bloc %in% c("CHINA", "EU")),
  
  trase_beef %>% 
    filter(!(str_detect(trase_id, "AGGREGATED"))) %>%
    group_by(year, economic_bloc, macro_basin) %>% 
    summarise(km3_gw = round(sum(pasture_gw_m3, na.rm = T)*10^-9),
              km3_bw = round(sum(tot_m3_zanetti, na.rm = T)*10^-9, 2)) %>% 
    ungroup() %>% 
    mutate(km3_tot_macro = round(km3_gw + km3_bw, 1)) %>% 
    filter(economic_bloc %in% c("CHINA", "EU"))) %>% 
  left_join(ws_labels %>% 
              rename(macro_basin = nm_macroRH)) %>% 
  left_join(summary_water_country %>% 
              select(year, economic_bloc, km3_tot)) %>% 
  mutate(pct_tot = round(100*km3_tot_macro/km3_tot, 2))

summary_water_country_macro_scarcity <- summary_water_country_macro %>% 
  group_by(year, economic_bloc, ana_water_scarcity) %>% 
  summarise(km3_gw = sum(km3_gw),
            km3_bw = sum(km3_bw),
            km3_tot = sum(km3_tot),
            pct_tot = round(sum(pct_tot))) %>% 
  ungroup()

View(summary_water_country_macro)
View(summary_water_country_macro_scarcity)

## derive volume of **blue water** for each macro basin and ANA
## water scarcity

summary_blue_water_country_macro <- summary_water_country_macro %>% 
  select(year:macro_basin, km3_bw, ana_water_scarcity) %>% 
  rename(km3_bw_macro = km3_bw) %>%
  left_join(ws_labels %>% 
              rename(macro_basin = nm_macroRH)) %>% 
  left_join(summary_water_country %>% 
              select(year, economic_bloc, km3_bw) %>% 
              rename(km3_bw_tot = km3_bw)) %>% 
  mutate(pct_tot = round(100*km3_bw_macro/km3_bw_tot, 2))

summary_blue_water_country_macro_scarcity <- summary_blue_water_country_macro %>% 
  group_by(year, economic_bloc, ana_water_scarcity) %>% 
  summarise(km3_bw = sum(km3_bw_macro),
            pct_tot = round(sum(pct_tot))) %>% 
  ungroup()

View(summary_blue_water_country_macro)
View(summary_blue_water_country_macro_scarcity)

# water savings -----------------------------------------------------------

## information in table 2

### soy

### obtain the median soy water footprint (green + blue)

median_soy_wf <- soy %>% 
  filter(soy_wf_rock != 0) %>% 
  group_by(year) %>% 
  summarise(median_wf = median(soy_wf_rock, na.rm = T)) %>% 
  ungroup()

### obtain the median beef water footprint (green + blue)

median_beef_wf <- beef %>% 
  filter(is.na(tot_m3_per_toncw_beef_dairy_anualpec_zanetti) == F, 
         tot_m3_per_toncw_beef_dairy_anualpec_zanetti != 0) %>% 
  group_by(year) %>% 
  summarise(median_wf = median(tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
                               na.rm = T)) %>% 
  ungroup()

### create function to determine water savings for China and the EU from soy

soy_water_savings <- function(y){
  
  trase_soy %>%
    filter(economic_bloc %in% c("CHINA", "EU")) %>% 
    left_join(soy_bench, by = c("year", "trase_id")) %>% 
    filter(tag_wf == "> cutoff WF",
           year == y) %>% 
    mutate(ref_wf = median_soy_wf$median_wf[median_soy_wf$year == y],
           m3_from_median_wf = signif(ref_wf*volume, 4),
           m3_ref = signif(soy_wf_rock.x*volume, 4)) %>% 
    group_by(economic_bloc) %>% 
    summarise(tot_km3_from_median_wf = 
                round(sum(m3_from_median_wf, na.rm = T)*10^-9, 2),
              tot_km3_ref = round(sum(m3_ref, na.rm = T)*10^-9, 2)) %>% 
    ungroup() %>% 
    mutate(median_wf = median_soy_wf$median_wf[median_soy_wf$year == y],
           km3_savings = round(tot_km3_ref - tot_km3_from_median_wf, 2),
           year = y,
           product = "Soy")
  
}

### create function to determine water savings for China and the EU from beef

beef_water_savings <- function(y){
  
  trase_beef %>%
    filter(economic_bloc %in% c("CHINA", "EU")) %>% 
    left_join(beef_bench, by = c("year", "trase_id")) %>% 
    filter(tag_wf == "> cutoff WF",
           year == y) %>% 
    mutate(ref_wf = median_beef_wf$median_wf[median_beef_wf$year == y],
           m3_from_median_wf = ref_wf*volume,
           m3_ref = tot_m3_per_toncw_beef_dairy_anualpec_zanetti.x*volume) %>% 
    group_by(economic_bloc) %>% 
    summarise(tot_km3_from_median_wf = sum(m3_from_median_wf, na.rm = T)*10^-9,
              tot_km3_ref = sum(m3_ref, na.rm = T)*10^-9) %>% 
    ungroup() %>% 
    mutate(median_wf = median_beef_wf$median_wf[median_beef_wf$year == y],
           km3_savings = round(tot_km3_ref - tot_km3_from_median_wf, 2),
           year = y,
           product = "Beef")
  
}

### create table 2

table2 <- rbind(
  soy_water_savings("2015"),
  soy_water_savings("2016"),
  soy_water_savings("2017"),
  beef_water_savings("2015"),
  beef_water_savings("2016"),
  beef_water_savings("2017")) %>% 
  arrange(economic_bloc) %>% 
  left_join(rbind(summary_water_country_soy %>% 
                    select(year, economic_bloc, product, km3_tot) %>% 
                    filter(economic_bloc != "BRAZIL") %>% 
                    mutate(type = "green + blue"),
                  summary_water_country_beef %>% 
                    select(year, economic_bloc, product, km3_bw) %>% 
                    rename(km3_tot = km3_bw) %>% 
                    mutate(type = "blue only"))) %>% 
  mutate(pct_savings = round(100*km3_savings/km3_tot))

View(table2)

# water scarcity and deforestation impacts --------------------------------

# information in table S14

## organize the data to derive 80th percentile production with largest
## water scarcity footprints

### create function to derive the water scarcity footprint benchmarks for each 
### country and year (consumption)

soy_benchmark_wsf <- function(country, y) {

  trase_soy %>%
  filter(economic_bloc == country,
         year == y,
         soy_bwsf_rock != 0) %>%
    group_by(year, economic_bloc, product, trase_id, soy_bwsf_rock) %>%
    summarise(volume = sum(volume, na.rm = T),
              ha_def = sum(ha_def, na.rm = T)) %>%
    ungroup() %>%
    arrange(soy_bwsf_rock) %>%
    mutate(volume_cum = cumsum(volume)) %>%
    inner_join(summary_water_country_product %>%
                 select(year, economic_bloc, product, vol_tot_irr)) %>%
  mutate(perc = volume_cum/vol_tot_irr) %>%
  filter(perc > 0.8)

}

beef_benchmark_wsf <- function(country, y) {

  trase_beef %>%
    filter(economic_bloc == country,
           year == y) %>%
    group_by(year, economic_bloc, product, trase_id, wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>%
    summarise(volume = sum(volume, na.rm = T),
              ha_def = sum(ha_def, na.rm = T)) %>%
    ungroup() %>%
    arrange(wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>%
    mutate(volume_cum = cumsum(volume)) %>%
    inner_join(summary_water_country_product %>%
                 select(year, economic_bloc, product, vol_tot)) %>%
    mutate(perc = volume_cum/vol_tot) %>%
    filter(perc > 0.8)

}

### create function to select municipalities supplying China and the EU that 
### have the largest deforestation footprint and either have irrigation (soy) or 
### the largest water scarcity footprint (beef) (production)

soy_df_wsf_bench <- function(country, y){
  
  trase_soy %>% 
  filter(economic_bloc == country,
         year == y) %>% 
  left_join(soy_bench) %>% 
  filter(tag_def == "> cutoff def",
         tag_irr == "irr") %>% 
    select(year, trase_id, economic_bloc, product, volume, ha_def)
  
}

beef_df_wsf_bench <- function(country, y){
  
  trase_beef %>% 
    filter(economic_bloc == country,
           year == y) %>% 
    left_join(beef_bench) %>% 
    filter(tag_def == "> cutoff def",
           tag_wsf == "> cutoff WSF") %>% 
    select(year, trase_id, economic_bloc, product, volume, ha_def)
  
} 

### now derive the first 2 rows of table s14
  
table_s14_top <- rbind(
  
  rbind(
    soy_benchmark_wsf("CHINA", "2017"),
    soy_benchmark_wsf("CHINA", "2016"),
    soy_benchmark_wsf("CHINA", "2015"),
    soy_benchmark_wsf("EU", "2017"),
    soy_benchmark_wsf("EU", "2016"),
    soy_benchmark_wsf("EU", "2015")) %>% 
    
    group_by(year, economic_bloc, product) %>% 
    summarise(Mt = sum(volume)*10^-6,
              ha_def = round(sum(ha_def))) %>% 
    ungroup(),
  
  rbind(
    beef_benchmark_wsf("CHINA", "2017"),
    beef_benchmark_wsf("CHINA", "2016"),
    beef_benchmark_wsf("CHINA", "2015"),
    beef_benchmark_wsf("EU", "2017"),
    beef_benchmark_wsf("EU", "2016"),
    beef_benchmark_wsf("EU", "2015")) %>% 
    
    group_by(year, economic_bloc, product) %>% 
    summarise(Mt = sum(volume)*10^-6,
              ha_def = round(sum(ha_def, na.rm = T))) %>% 
    ungroup()) %>% 
  
  left_join(summary_water_country_product %>% 
              select(year, economic_bloc, product, ha_def_tot)) %>% 
  mutate(perc_def = round(100*ha_def/ha_def_tot))
  
View(table_s14_top)

## and the bottom 2 rows of table s14

table_s14_bottom <- rbind(
  
  rbind(
    soy_df_wsf_bench("CHINA", "2017"),
    soy_df_wsf_bench("CHINA", "2016"),
    soy_df_wsf_bench("CHINA", "2015"),
    soy_df_wsf_bench("EU", "2017"),
    soy_df_wsf_bench("EU", "2016"),
    soy_df_wsf_bench("EU", "2015")),
  
  rbind(
    beef_df_wsf_bench("CHINA", "2017"),
    beef_df_wsf_bench("CHINA", "2016"),
    beef_df_wsf_bench("CHINA", "2015"),
    beef_df_wsf_bench("EU", "2017"),
    beef_df_wsf_bench("EU", "2016"),
    beef_df_wsf_bench("EU", "2015"))) %>% 
    
    group_by(year, economic_bloc, product) %>% 
    summarise(Mt = round(sum(volume)*10^-6, 6),
              ha_def = round(sum(ha_def, na.rm = T)),
              n_municipalities = n_distinct(trase_id)) %>% 
    ungroup() %>% 
  arrange(desc(product), year) %>% 
  left_join(summary_water_country_product %>% 
              select(year, economic_bloc, product, ha_def_tot)) %>% 
  mutate(perc_def = round(100*ha_def/ha_def_tot))

View(table_s14_bottom)
