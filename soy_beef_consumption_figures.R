# Script to produce soy and beef consumption figures for:
# ... "International appropriation of Brazil's water through soy and beef supply chains"
# 
# Author: Michael J. Lathuillière
# Contact michael.lathuilliere@sei.org with any questions

# Workstation set up ------------------------------------------------------

# Load packages
library(tidyverse)
library(sf)
library(ggthemes)
library(patchwork)

# add fonts
windowsFonts(Calibri = windowsFont("Calibri"))

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


# map China and EU sourcing of soy and beef per macro basin ---------------

## plot figure 4 (bar graphs), S10 and S11

## these graphs show panels A and B of above figures

### combine the soy and beef commodities and then add the proper labels
### green and blue water combined
### consider both meso and macro basins

### add product to each trade dataset

trase_soy <- trase_soy %>% 
  mutate(product = "Soy")

trase_beef <- trase_beef %>% 
  mutate(product = "Beef")

com_all <- rbind(
  
  trase_soy %>% 
    select(year, trase_id, name, state, sigla_uf, macro_basin, economic_bloc, m3_bw_rock, product) %>% 
    rename(water_source = m3_bw_rock) %>% 
    mutate(water_type = "Blue"),
  
  trase_beef %>% 
    select(year, trase_id, name, state, sigla_uf, macro_basin, economic_bloc, tot_m3_zanetti, product) %>% 
    rename(water_source = tot_m3_zanetti) %>% 
    mutate(water_type = "Blue"),
  
  trase_soy %>% 
    select(year, trase_id, name, state, sigla_uf, macro_basin, economic_bloc, m3_gw_rock, product) %>% 
    rename(water_source = m3_gw_rock) %>% 
    mutate(water_type = "Green"),
  
  trase_beef %>% 
    select(year, trase_id, name, state, sigla_uf, macro_basin, economic_bloc, pasture_gw_m3, product) %>% 
    rename(water_source = pasture_gw_m3) %>% 
    mutate(water_type = "Green")) %>% 
  
  filter(economic_bloc %in% c("CHINA", "EU"), 
         water_source != 0) %>% 
  mutate(type = factor(paste(product, "-", water_type), 
                       levels = c("Soy - Green",
                                  "Beef - Green",
                                  "Soy - Blue",
                                  "Beef - Blue"))) %>% 
  group_by(year, trase_id, name, state, sigla_uf, macro_basin, economic_bloc, type) %>% 
  summarise(km3_water_source = sum(water_source, na.rm = T)*10^-9) %>% 
  ungroup() %>% 
  arrange(factor(type))

### provide summary of water use to plot axes

summary_water_country_macro <- com_all %>%
  filter(!(str_detect(trase_id, "AGGREGATED")),
         economic_bloc %in% c("CHINA", "EU")) %>%    
  group_by(year, macro_basin, economic_bloc) %>% 
  summarise(km3_water_source_tot = round(sum(km3_water_source, na.rm = T), 2)) %>% 
  ungroup() %>% 
  mutate(type = "Green + Blue")

summary_water_country_macro_blue <- com_all %>%
  filter(!(str_detect(trase_id, "AGGREGATED")),
         economic_bloc %in% c("CHINA", "EU"),
         type %in% c("Soy - Blue", "Beef - Blue")) %>%    
  group_by(year, macro_basin, economic_bloc) %>% 
  summarise(km3_water_source_tot = round(sum(km3_water_source, na.rm = T), 2)) %>% 
  ungroup() %>% 
  mutate(type = "Blue")

### create basin list

basin_all <- com_all %>% 
  filter(is.na(macro_basin) == F) %>% 
  distinct(macro_basin)


### create a function to select basin order

list_basin_order <- function(y, country){
  
  # separate the year and country of interest
  t <- com_all %>%
    filter(!(str_detect(trase_id, "AGGREGATED"))) %>%    
    filter(year == y,                          
           economic_bloc == country) %>%       
    group_by(year, type, macro_basin) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T))
  
  # establish missing basins and count their number
  missing_basins <- setdiff(basin_all$macro_basin, t$macro_basin)
  
  n <- length(missing_basins)
  
  # add missing basins with no data (i.e. 0 km3 of water)
  u <- tibble()
  
  if(n != 0) {
    
    for (i in 1:n){
      
      yy <- tibble_row(
        year = y,
        type = "Beef - Blue", 
        macro_basin = missing_basins[i],
        km3_water_source = 0)
      
      u <- rbind(u, yy)
      
    } 
  }
  
  v <- t %>%
    rbind(u) %>% 
    group_by(macro_basin) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(km3_water_source)
  
  v$macro_basin
  
}

### plot the left hand side of the graph (green + blue water) - panel A

plot_basin_links_left <- function(y, country, order){
  
  # separate the year and country of interest
  t <- com_all %>%
    filter(!(str_detect(trase_id, "AGGREGATED"))) %>% 
    filter(year == y,                                    
           economic_bloc == country) %>%                        
    group_by(year, type, macro_basin) %>%         
    summarise(km3_water_source = sum(km3_water_source, na.rm = T)) %>% 
    ungroup()
  
  # establish missing basins and count their number
  missing_basins <- setdiff(basin_all$macro_basin, t$macro_basin)
  
  n <- length(missing_basins)
  
  # add missing basins with no data (i.e. 0 km3 of water)
  u <- tibble()
  
  if(n != 0) {
    
    for (i in 1:n){
      
      yy <- tibble_row(
        year = y,                      
        type = "Beef - Blue",                       # this is just a placeholder, the volumes are 0 anyways here
        macro_basin = missing_basins[i], 
        km3_water_source = 0)
      
      u <- rbind(u, yy)
      
    } 
  }
  
  t %>%
    rbind(u) %>% 
    ggplot() + 
    geom_bar(aes(x = factor(macro_basin, levels = list_basin_order(y, order)),         
                 y = km3_water_source, 
                 fill = factor(type, levels = c("Beef - Blue", 
                                                 "Soy - Blue",
                                                 "Beef - Green",
                                                 "Soy - Green"))),  
             position = 'stack',
             stat = 'identity') +
    coord_flip() + 
    labs(title = "A",
         subtitle = paste(country),      
         x = "",
         y = bquote('Water ('*km^{3}*')'))  +
    scale_y_reverse() +
    scale_x_discrete(position = 'top') +
    theme_tufte() +
    scale_fill_manual(values = c("Soy - Green" = "#004d33", 
                                 "Beef - Green" = "#00d29a",
                                 "Soy - Blue" = "#194179",
                                 "Beef - Blue" = "#4aa2d0"),
                      labels  = c("Soy - Green" = "Soy, green", 
                                  "Beef - Green" = "Beef, green",
                                  "Soy - Blue" = "Soy, blue", 
                                  "Beef - Blue" = "Beef, blue"),
                      guide = guide_legend(reverse = TRUE)) +
    ylim(max(summary_water_country_macro$km3_water_source_tot ), 0) +         
    theme(axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 5),
          axis.text.y = element_text(size = 5, hjust = 1, vjust = 0.5),
          axis.ticks.y = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 5),
          legend.title = element_blank(),
          legend.key.size = unit(0.15, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.title = element_text(size = 8, hjust = 0),
          plot.subtitle = element_text(size = 6, hjust = 1),
          text = element_text(family = "Calibri"))
  
}

### plot the right hand side of the graph (green + blue water) - panel A

plot_basin_links_right <- function(y, country, order){
  
  # separate the year and country of interest
  t <- com_all %>%
    filter(!(str_detect(trase_id, "AGGREGATED"))) %>%    #remove flows that cannot be linked to basin
    filter(year == y, 
           economic_bloc == country) %>% 
    group_by(year, type, macro_basin) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T)) %>% 
    ungroup()
  
  # establish missing basins and count their number
  missing_basins <- setdiff(basin_all$macro_basin, t$macro_basin)
  
  n <- length(missing_basins)
  
  # add missing basins with no data (i.e. 0 km3 of water)
  u <- tibble()
  
  if(n != 0) {
    
    for (i in 1:n){
      
      yy <- tibble_row(
        year = y,
        type = "Beef - Blue",                         # this is just a placeholder, the volumes are 0 anyways here
        macro_basin = missing_basins[i],
        km3_water_source = 0)
      
      u <- rbind(u, yy)
      
    } 
  }
  
  t %>% 
    rbind(u) %>% 
    ggplot() + 
    geom_bar(aes(x = factor(macro_basin, levels = list_basin_order(y, order)), 
                 y = km3_water_source, 
                 fill = factor(type, levels = c("Beef - Blue", 
                                                "Soy - Blue",
                                                "Beef - Green",
                                                "Soy - Green"))), 
             position = 'stack',
             stat = 'identity') +
    coord_flip() + 
    labs(title = "",
         subtitle = paste(country),
         x = "", 
         y = bquote('Water ('*km^{3}*')'))  +
    theme_tufte() +
    scale_fill_manual(values = c("Soy - Green" = "#004d33", 
                                 "Beef - Green" = "#00d29a",
                                 "Soy - Blue" = "#194179",
                                 "Beef - Blue" = "#4aa2d0"),
                      labels  = c("Soy - Green" = "Soy, green", 
                                  "Beef - Green" = "Beef, green",
                                  "Soy - Blue" = "Soy, blue", 
                                  "Beef - Blue" = "Beef, blue"),
                      guide = guide_legend(reverse = TRUE)) +
    ylim(0, max(summary_water_country_macro$km3_water_source_tot )) +
    theme(axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 5),
          legend.title = element_blank(),
          legend.key.size = unit(0.15, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.subtitle = element_text(size = 6),
          text = element_text(family = "Calibri"))
  
}

### plot the left hand side of the graph (blue water) - panel B

plot_basin_links_left_blue <- function(y, country, order){
  
  # separate the year and country of interest
  t <- com_all %>%
    filter(!(str_detect(trase_id, "AGGREGATED")),
           type %in% c("Soy - Blue", "Beef - Blue")) %>%    
    filter(year == y,                               
           economic_bloc == country) %>%              
    group_by(year, type, macro_basin) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T))
  
  # establish missing basins and count their number
  missing_basins <- setdiff(basin_all$macro_basin, t$macro_basin)
  
  n <- length(missing_basins)
  
  # add missing basins with no data (i.e. 0 km3 of water)
  u <- tibble()
  
  if(n != 0) {
    
    for (i in 1:n){
      
      yy <- tibble_row(
        year = y,                      
        type = "Beef - Blue", 
        macro_basin = missing_basins[i],
        km3_water_source = 0)
      
      u <- rbind(u, yy)
      
    } 
  }
  
  t %>%
    rbind(u) %>% 
    ggplot() + 
    geom_bar(aes(x = factor(macro_basin, levels = list_basin_order(y, order)),      
                 y = km3_water_source, 
                 fill = factor(type, levels = c("Beef - Blue", 
                                                "Soy - Blue"))), 
             position = 'stack',
             stat = 'identity') +
    coord_flip() + 
    labs(title = "B",
         subtitle = paste(country),                  
         x = "", 
         y = bquote('Blue water ('*km^{3}*')'))  +
    scale_y_reverse() +
    scale_x_discrete(position = 'top') +
    theme_tufte() +
    scale_fill_manual(values = c("Soy - Blue" = "#194179",
                                 "Beef - Blue" = "#4aa2d0"),            
                      labels  = c("Soy - Blue" = "Soy, blue", 
                                  "Beef - Blue" = "Beef, blue"),
                      guide = guide_legend(reverse = TRUE)) +
    ylim(max(summary_water_country_macro_blue$km3_water_source_tot ), 0) +
    theme(axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 5),
          axis.text.y = element_text(size = 5, hjust = 1, vjust = 0.5),
          axis.ticks.y = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 5),
          legend.title = element_blank(),
          legend.key.size = unit(0.15, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.title = element_text(size = 8, hjust = 0),
          plot.subtitle = element_text(size = 6, hjust = 1),
          text = element_text(family = "Calibri"))
  
}

### plot the right hand side of the graph (blue water) - panel B

plot_basin_links_right_blue <- function(y, country, order){
  
  # separate the year and country of interest
  t <- com_all %>%
    filter(!(str_detect(trase_id, "AGGREGATED")),
           type %in% c("Soy - Blue", "Beef - Blue")) %>%    
    filter(year == y, 
           economic_bloc == country) %>% 
    group_by(year, type, macro_basin) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T))
  
  # establish missing basins and count their number
  missing_basins <- setdiff(basin_all$macro_basin, t$macro_basin)
  
  n <- length(missing_basins)
  
  # add mising basins with no data (i.e. 0 km3 of water)
  u <- tibble()
  
  if(n != 0) {
    
    for (i in 1:n){
      
      yy <- tibble_row(
        year = y,
        type = "Beef - Blue", 
        macro_basin = missing_basins[i],
        km3_water_source = 0)
      
      u <- rbind(u, yy)
      
    } 
  }
  
  t %>% 
    rbind(u) %>% 
    ggplot() + 
    geom_bar(aes(x = factor(macro_basin, levels = list_basin_order(y, order)), 
                 y = km3_water_source, 
                 fill = factor(type, levels = c("Beef - Blue", 
                                                "Soy - Blue"))), 
             position = 'stack',
             stat = 'identity') +
    coord_flip() + 
    labs(title = "",
         subtitle = paste(country),
         x = "", 
         y = bquote('Blue water ('*km^{3}*')'))  +
    theme_tufte() +
    scale_fill_manual(values = c("Soy - Blue" = "#194179",
                                 "Beef - Blue" = "#4aa2d0"),
                      labels  = c("Soy - Blue" = "Soy, blue", 
                                  "Beef - Blue" = "Beef, blue"),
                      guide = guide_legend(reverse = TRUE)) +
    ylim(0, max(summary_water_country_macro_blue$km3_water_source_tot )) +
    theme(axis.text.x = element_text(size = 5),
          axis.title.x = element_text(size = 5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 5),
          legend.title = element_blank(),
          legend.key.size = unit(0.15, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.subtitle = element_text(size = 6, hjust = 0),
          text = element_text(family = "Calibri"))
  
}

## build figures 4, S10 and S11, panels A and B

fig4_2017 <- (plot_basin_links_left("2017", "CHINA", "CHINA") + 
                plot_spacer() + 
                plot_basin_links_right("2017", "EU", "CHINA") +
                plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
                theme(legend.position = 'bottom')) /
  
  (plot_basin_links_left_blue("2017", "CHINA", "CHINA") + 
     plot_spacer() + 
     plot_basin_links_right_blue("2017", "EU", "CHINA") +
     plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
     theme(legend.position = 'bottom'))

figS10_2015 <- (plot_basin_links_left("2015", "CHINA", "CHINA") + 
                  plot_spacer() + 
                  plot_basin_links_right("2015", "EU", "CHINA") +
                  plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
                  theme(legend.position = 'bottom')) /
  
  (plot_basin_links_left_blue("2015", "CHINA", "CHINA") + 
     plot_spacer() + 
     plot_basin_links_right_blue("2015", "EU", "CHINA") +
     plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
     theme(legend.position = 'bottom'))

figS11_2016 <- (plot_basin_links_left("2016", "CHINA", "CHINA") + 
                plot_spacer() + 
                plot_basin_links_right("2016", "EU", "CHINA") +
                plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
                theme(legend.position = 'bottom')) /
  
  (plot_basin_links_left_blue("2016", "CHINA", "CHINA") + 
     plot_spacer() + 
     plot_basin_links_right_blue("2016", "EU", "CHINA") +
     plot_layout(widths = c(4, -1.8 , 4), guides = 'collect', nrow = 1) &
     theme(legend.position = 'bottom'))

## export to tmp

# ggsave(paste0(tmp, "_", Sys.Date(), "_fig4_bargraph_2017.png"),
#      width = 11,
#      height = 11,
#      units = "cm",
#      plot = fig4_2017,
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figS10_bargraph_2015.png"),
#        width = 11,
#        height = 11,
#        units = "cm",
#        plot = figS10_2015,
#        dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figS11_bargraph_2016.png"),
#        width = 11,
#        height = 11,
#        units = "cm",
#        plot = figS11_2016,
#        dpi = 300)

## plot figure 4 (maps), S10 and S11

## these graphs show Panel C and D of above figures

plot_bw_map_left <- function(y, panel){
  
  com_all %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(!(str_detect(trase_id, "AGGREGATED")), 
           is.na(year) == T | year == y,                     
           economic_bloc == "CHINA",           
           type %in% c("Soy - Blue", "Beef - Blue")) %>% 
    group_by(trase_id, geometry) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(tag = case_when(
      km3_water_source > 0 & km3_water_source < 10^-6 ~ "0-10^-6",
      km3_water_source >= 10^-6 & km3_water_source < 10^-4 ~ "10^-6-10^-4",
      km3_water_source >= 10^-4 & km3_water_source < 10^-2 ~ "10^-4-10^-2",
      km3_water_source >= 10^-2 ~ ">10^-2",
      TRUE ~NA)) %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = basin_map, aes(fill = nm_macroRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "CHINA", size = unit(2, "pt")) +       ##
    scale_fill_manual(~km^{3}* ~'y'^{-1},
                      breaks = c("0-10^-6", "10^-6-10^-4", "10^-4-10^-2", ">10^-2"),
                      values = c("0-10^-6" = "#85bedf",
                                 "10^-6-10^-4" = "#4188bb",
                                 "10^-4-10^-2" = "#2b5798",
                                 ">10^-2" = "#062d66"),
                      labels  = c("0-10^-6" = expression(paste(" ", "0-", 10^{-6})),
                                  "10^-6-10^-4" = expression(paste(" ", 10^{-6}, "-", 10^{-4})),
                                  "10^-4-10^-2" = expression(paste(" ", 10^{-4}, "-", 10^{-2})),
                                  ">10^-2" = expression(paste(">", 10^{-2})))) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = panel) +  
    ylab("") + xlab("") + 
    theme_tufte() +
    theme(
      plot.title = element_text(size = 8),
      axis.text.y = element_text(size = 5), 
      axis.text.x = element_text(size = 5),
      axis.ticks.y = element_line(linewidth = 0.1), 
      axis.ticks.x = element_line(linewidth = 0.1), 
      legend.position = "bottom",
      legend.title = element_text(size = 8, margin = margin(r = 0.05, b = 0.05, unit = 'cm'), hjust = -1),
      legend.text = element_text(size = 8, margin = margin(l = 0.025, unit = 'cm')),
      legend.key.size = unit(0.35, "cm"),
      legend.key = element_rect(colour = 'black', linewidth = 0.1),
      text = element_text(family = "Calibri"))
  
}

plot_bw_map_right <- function(y, panel){
  
  com_all %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(!(str_detect(trase_id, "AGGREGATED")), 
           is.na(year) == T | year == y,                     
           economic_bloc == "EU",           
           type %in% c("Soy - Blue", "Beef - Blue")) %>% 
    group_by(trase_id, geometry) %>% 
    summarise(km3_water_source = sum(km3_water_source, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(tag = case_when(
      km3_water_source > 0 & km3_water_source < 10^-6 ~ "0-10^-6",
      km3_water_source >= 10^-6 & km3_water_source < 10^-4 ~ "10^-6-10^-4",
      km3_water_source >= 10^-4 & km3_water_source < 10^-2 ~ "10^-4-10^-2",
      km3_water_source >= 10^-2 ~ ">10^-2",
      TRUE ~NA)) %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = basin_map, aes(fill = nm_macroRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "EU", size = unit(2, "pt")) +       ##
    scale_fill_manual(~km^{3}* ~'y'^{-1},
                      breaks = c("0-10^-6", "10^-6-10^-4", "10^-4-10^-2", ">10^-2"),
                      values = c("0-10^-6" = "#85bedf",
                                 "10^-6-10^-4" = "#4188bb",
                                 "10^-4-10^-2" = "#2b5798",
                                 ">10^-2" = "#062d66"),
                      labels  = c("0-10^-6" = expression(paste(" ", "0-", 10^{-6})),
                                  "10^-6-10^-4" = expression(paste(" ", 10^{-6}, "-", 10^{-4})),
                                  "10^-4-10^-2" = expression(paste(" ", 10^{-4}, "-", 10^{-2})),
                                  ">10^-2" = expression(paste(">", 10^{-2})))) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = panel) + 
    ylab("") + xlab("") +
    theme_tufte() +
    theme(
      plot.title = element_text(size = 8),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_blank(), 
      axis.ticks.x = element_line(linewidth = 0.1),
      axis.ticks.y = element_blank(), 
      legend.title = element_text(size = 8, margin = margin(r = 0.05, b = 0.05, unit = 'cm'), hjust = -1),
      legend.text = element_text(size = 8, margin = margin(l = 0.025, unit = 'cm')),
      legend.key.size = unit(0.35, "cm"),
      legend.key = element_rect(colour = 'black', linewidth = 0.1),
      text = element_text(family = "Calibri"))
  
}

## build figures 4, S10 and S11, panels C and D

fig4_map_2017 <- plot_bw_map_left("2017", "C") + plot_bw_map_right("2017", "D") + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

figs10_map_2015 <- plot_bw_map_left("2015", "C") + plot_bw_map_right("2015", "D") +
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

figs11_map_2016 <- plot_bw_map_left("2016", "C") + plot_bw_map_right("2016", "D") +
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

## export to tmp

# ggsave(paste0(tmp, "_", Sys.Date(), "_fig4_map_2017.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = fig4_map_2017,
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figs10_map_2015.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = figs10_map_2015,
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figs11_map_2016.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = figs11_map_2016,
#      dpi = 300)

# map China and EU soy water scarcity footprint from irrigation -----------

## soy

## plot figure 5 (maps), S14 and S15

## these graphs show panels C and D of above figures

# derive some important national import information for plots
imp_tot_cn_eu <- trase_soy %>%
  group_by(year, economic_bloc) %>% 
  summarise(vol_tot = sum(volume, na.rm = T)) %>% 
  ungroup() %>% 
  filter(economic_bloc %in% c("CHINA", "EU"))

imp_tot_cn_eu_irr <- trase_soy %>%
  filter(soy_bwsf_rock != 0) %>% 
  group_by(year, economic_bloc) %>% 
  summarise(vol_tot_irr = sum(volume, na.rm = T)) %>% 
  ungroup() %>% 
  filter(economic_bloc %in% c("CHINA", "EU")) %>% 
  left_join(imp_tot_cn_eu) %>% 
  mutate(pct_irr = 100*vol_tot_irr/vol_tot)

plot_soy_wsf_benchmark <- function(y) {
  
  t_cn <- trase_soy %>% 
    filter(year == y,
           economic_bloc == "CHINA",
           soy_bwsf_rock != 0) %>% 
    group_by(year, trase_id, economic_bloc, soy_bwf_rock, soy_bwsf_rock) %>% 
    summarise(volume = sum(volume, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(soy_bwsf_rock) %>% 
    mutate(volume_cum = cumsum(volume)) %>% 
    inner_join(imp_tot_cn_eu_irr %>% 
                 select(year, economic_bloc, vol_tot_irr), by = c("year", "economic_bloc")) %>% 
    mutate(perc = volume_cum/vol_tot_irr) %>% 
    select(year, trase_id, economic_bloc, volume, volume_cum, perc, soy_bwf_rock, soy_bwsf_rock, vol_tot_irr) %>% 
    filter(perc > 0.80) %>% 
    merge(br %>% 
            select(trase_id, geometry), by = "trase_id")
    
  t_eu <- trase_soy %>% 
    filter(year == y,
           economic_bloc == "EU",
           soy_bwsf_rock != 0) %>% 
    group_by(year, trase_id, economic_bloc, soy_bwf_rock, soy_bwsf_rock) %>% 
    summarise(volume = sum(volume, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(soy_bwsf_rock) %>% 
    mutate(volume_cum = cumsum(volume)) %>% 
    inner_join(imp_tot_cn_eu_irr %>% 
                 select(year, economic_bloc, vol_tot_irr), by = c("year", "economic_bloc")) %>% 
    mutate(perc = volume_cum/vol_tot_irr) %>% 
    select(year, trase_id, economic_bloc, volume, volume_cum, perc, soy_bwf_rock, soy_bwsf_rock, vol_tot_irr) %>% 
    filter(perc > 0.80) %>% 
    merge(br %>% 
            select(trase_id, geometry), by = "trase_id")
  
  left_cn <- soy %>% 
    merge(br %>%                                           
            select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>%
    mutate(wsf_bin = case_when(
      is.na(soy_bwsf_rock) == T | soy_bwsf_rock == 0 ~ "0",
      soy_bwsf_rock > 0 & soy_bwsf_rock < 500 ~ "0-500",
      soy_bwsf_rock >= 500 & soy_bwsf_rock < 1000 ~ "500-1000",
      soy_bwsf_rock >= 1000 & soy_bwsf_rock < 2000 ~ "1000-2000",
      soy_bwsf_rock >= 2000 & soy_bwsf_rock < 3000 ~ "2000-3000",
      soy_bwsf_rock >= 3000 & soy_bwsf_rock < 5000 ~ "3000-5000",
      soy_bwsf_rock >= 5000 ~ "> 5000")) %>% 
    ggplot() +
    geom_sf(aes(fill = wsf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = t_cn, aes(geometry = geometry), fill = NA, linewidth = 0.15, col = "#0068B9") +
    geom_sf(data = basin_map, aes(fill = nm_macroRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "CHINA", size = unit(3, "pt")) +
    scale_fill_manual(~m^{3}* ~'t'^{-1},
                      breaks = c("0", "0-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "> 5000"),
                      values = c("0" = 'white', 
                                 "0-500" = "#ffe35f",
                                 "500-1000" = "#fec44f",
                                 "1000-2000" = "#f09f50",
                                 "2000-3000" = "#cb662e",
                                 "3000-5000" = "#aa4512",
                                 "> 5000" = "#8f3103"))  +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "C") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(linewidth = 0.1),
          axis.title = element_blank(),
          legend.title = element_text(size = 8, margin = margin(b = 0.1, unit = 'cm'), hjust = -0.05), 
          legend.text = element_text(size = 8, margin = margin(l = 0.1, unit = 'cm')),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          text = element_text(family = "Calibri"))
  
  
  right_eu <- soy %>% 
    merge(br %>%                                                  
            select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>%
    mutate(wsf_bin = case_when(
      is.na(soy_bwsf_rock) == T | soy_bwsf_rock == 0 ~ "0",
      soy_bwsf_rock > 0 & soy_bwsf_rock < 500 ~ "0-500",
      soy_bwsf_rock >= 500 & soy_bwsf_rock < 1000 ~ "500-1000",
      soy_bwsf_rock >= 1000 & soy_bwsf_rock < 2000 ~ "1000-2000",
      soy_bwsf_rock >= 2000 & soy_bwsf_rock < 3000 ~ "2000-3000",
      soy_bwsf_rock >= 3000 & soy_bwsf_rock < 5000 ~ "3000-5000",
      soy_bwsf_rock >= 5000 ~ "> 5000")) %>% 
    ggplot() +
    geom_sf(aes(fill = wsf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = t_eu, aes(geometry = geometry), fill = NA, linewidth = 0.15, col = "#0068B9") +
    geom_sf(data = basin_map, aes(fill = nm_macroRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "EU", size = unit(3, "pt")) +
    scale_fill_manual(~m^{3}* ~'t'^{-1},
                      breaks = c("0", "0-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "> 5000"),
                      values = c("0" = 'white', 
                                 "0-500" = "#ffe35f",
                                 "500-1000" = "#fec44f",
                                 "1000-2000" = "#f09f50",
                                 "2000-3000" = "#cb662e",
                                 "3000-5000" = "#aa4512",
                                 "> 5000" = "#8f3103"))  +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "D") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(size = 0.1),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_text(size = 8, margin = margin(b = 0.1, unit = 'cm'), hjust = -0.05), 
          legend.text = element_text(size = 8, margin = margin(l = 0.1, unit = 'cm')),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.margin = margin(r = 0.15, unit = 'cm'),
          text = element_text(family = "Calibri"))
  
  
  plot_figure <- left_cn + right_eu + plot_layout(guides = "collect") &
    theme(legend.position = 'right')
  
  plot_figure
  
}

# export these maps as bottom panel of figure 5 (2017), S14 (2015), S15 (2016) 

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure5_soy_wsf_2017_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_soy_wsf_benchmark("2017"),
#     dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figure5_soy_wsf_2015_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_soy_wsf_benchmark("2015"),
#     dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figure5_soy_wsf_2016_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_soy_wsf_benchmark("2016"),
#     dpi = 300)

## beef

## plot figure 6 (maps), S16 and S17

## these graphs show panels C and D of above figures

# derive some important national import information (removing unknowns)
imp_tot_cn_eu <- trase_beef %>%
  group_by(year, economic_bloc) %>% 
  summarise(vol_tot = sum(volume, na.rm = T)) %>% 
  ungroup() %>% 
  filter(economic_bloc %in% c("CHINA", "EU"))

plot_wsf_beef <- function(y) {
  
  t_cn <-   trase_beef %>%
    filter(year == y,
           economic_bloc == "CHINA") %>% 
    group_by(year, trase_id, economic_bloc, 
             tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
             wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    summarise(volume = sum(volume, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    mutate(volume_cum = cumsum(volume)) %>% 
    inner_join(imp_tot_cn_eu %>% 
                 select(year, economic_bloc, vol_tot), by = c("year", "economic_bloc")) %>% 
    mutate(perc = volume_cum/vol_tot) %>% 
    select(year, trase_id, economic_bloc, volume, volume_cum, perc, 
           tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
           wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
           vol_tot) %>% 
    filter(perc > 0.80) %>% 
    merge(br %>% 
            select(trase_id, geometry), by = "trase_id")
  
  t_eu <- trase_beef %>%
    filter(year == y,
           economic_bloc == "EU") %>% 
    group_by(year, trase_id, economic_bloc, 
             tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
             wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    summarise(volume = sum(volume, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    mutate(volume_cum = cumsum(volume)) %>% 
    inner_join(imp_tot_cn_eu %>% 
                 select(year, economic_bloc, vol_tot), by = c("year", "economic_bloc")) %>% 
    mutate(perc = volume_cum/vol_tot) %>% 
    select(year, trase_id, economic_bloc, volume, volume_cum, perc, 
           tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
           wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti, 
           vol_tot) %>% 
    filter(perc > 0.80) %>% 
    merge(br %>% 
            select(trase_id, geometry), by = "trase_id")
  
  left_cn <- beef %>% 
    rename(wsf = wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>%
    mutate(wsf_bin = case_when(
      is.na(wsf) == T | wsf == 0 ~ "0",
      wsf > 0 & wsf < 10000 ~ "0-10k",
      wsf >= 10000 & wsf < 50000 ~ "10-50k",
      wsf >= 50000 & wsf < 100000 ~ "50-100k",
      wsf >= 100000 ~ ">100k")) %>% 
    ggplot() +
    geom_sf(aes(fill = wsf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = t_cn, aes(geometry = geometry), fill = NA, size = 0.15, col = "#0068B9") +
    geom_sf(data = basin_map, aes(fill = nm_mesoRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "CHINA", size = unit(2, "pt")) +
    scale_fill_manual(~m^{3}* ~'(t CW)'^{-1},
                      breaks = c("0", "0-10k", "10-50k", "50-100k", ">100k"),
                      values = c("0" = 'white',
                                 "0-10k" = "#fec44f",
                                 "10-50k" = "#f09f50",
                                 "50-100k" = "#cb662e",
                                 #"50-100k" = "#aa4512",
                                 #"40000-50000" = "#F85E6E",
                                 ">100k" = "#8F3103")) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "C") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(linewidth = 0.1),
          axis.title = element_blank(),
          legend.title = element_text(size = 8, margin = margin(b = 0.1, unit = 'cm'), hjust = 0.5), 
          legend.text = element_text(size = 8, margin = margin(l = 0.1, unit = 'cm')),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          text = element_text(family = "Calibri"))
  
  
  right_eu <- beef %>% 
    rename(wsf = wsf_tot_m3_per_toncw_beef_dairy_anualpec_zanetti) %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>%
    mutate(wsf_bin = case_when(
      is.na(wsf) == T | wsf == 0 ~ "0",
      wsf > 0 & wsf < 10000 ~ "0-10k",
      wsf >= 10000 & wsf < 50000 ~ "10-50k",
      wsf >= 50000 & wsf < 100000 ~ "50-100k",
      wsf >= 100000 ~ ">100k")) %>% 
    ggplot() +
    geom_sf(aes(fill = wsf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = t_eu, aes(geometry = geometry), fill = NA, size = 0.15, col = "#0068B9") +
    geom_sf(data = basin_map, aes(fill = nm_mesoRH, geometry = geometry), fill = NA, size = 1.5, col = 'black') +
    annotate("text", x = -40, y = 0, label= "EU", size = unit(2, "pt")) +
    scale_fill_manual(~m^{3}* ~'(t CW)'^{-1},
                      breaks = c("0", "0-10k", "10-50k", "50-100k", ">100k"),
                      values = c("0" = 'white',
                                 "0-10k" = "#fec44f",
                                 "10-50k" = "#f09f50",
                                 "50-100k" = "#cb662e",
                                 #"50-100k" = "#aa4512",
                                 #"40000-50000" = "#F85E6E",
                                 ">100k" = "#8F3103")) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "D") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(linewidth =  0.1),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_text(size = 8, margin = margin(b = 0.1, unit = 'cm'), hjust = 0.5), 
          legend.text = element_text(size = 8, margin = margin(l = 0.1, unit = 'cm')),
          legend.key.size = unit(0.25, "cm"),
          legend.key = element_rect(colour = 'black', linewidth = 0.1),
          plot.margin = margin(r = 0.15, unit = 'cm'),
          text = element_text(family = "Calibri"))
  
  
  plot_figure <- left_cn + right_eu + plot_layout(guides = "collect") &
    theme(legend.position = 'right')
  
  plot_figure
  
}

# export these maps as bottom panel of figure 6 (2017), S16 (2015), S17 (2016) 

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure6_beef_wsf_2017_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_wsf_beef("2017"),
#     dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figure6_beef_wsf_2015_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_wsf_beef("2015"),
#     dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figure6_beef_wsf_2016_CD.png"),
#     width = 18,
#     height = 11,
#     units = "cm",
#     plot = plot_wsf_beef("2016"),
#     dpi = 300)
