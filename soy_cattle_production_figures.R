# Script to produce soy and cattle production figures for:
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
load(file= paste0(tmp, "soy_cattle_production.RData"))

# the above file loads the following data objects: 

## map information
### br - the map of Brazil (IBGE)
### mun - Brazilian municipalities, their ids and boundaries (IBGE)
### br_regions - the 5 regions of Brazil (IBGE)
### br_basins - the macro basins of Brazil according to the Brazilian National
###             Water Agency (ANA)

## data
### soy - soy production and performance per municipality for 2015-2017
### cattle - beef production (as live weight) and performance per municipality 
###          for 2010-2017
### soy_bench - soy production benchmarks
### cattle_bench - beef production benchmarks (as living herd in live weight)


# map soy production water footprints -------------------------------------

# create 2 maps side by side with total and blue water

# create inset with regions

map_inset <- ggplot() + 
  geom_sf(data = br_regions, fill = NA, size = 1.5, col = 'black') + 
  annotate("text", x = -60, y = -4, label = "bold(N)", size = 1, parse = TRUE) + 
  annotate("text", x = -42, y = -8, label = "bold(NE)", size = 1, parse = TRUE) + 
  annotate("text", x = -54, y = -15, label = "bold(CWe)", size = 1, parse = TRUE) + 
  annotate("text", x = -45, y = -20, label = "bold(SE)", size = 1, parse = TRUE) + 
  annotate("text", x = -52, y = -28, label = "bold(S)", size = 1, parse = TRUE) +
  ylim(-33.75077, 5.271841) + 
  xlim(-73.99045, -29.30279) +
  xlab("") + ylab("") + 
  theme_tufte() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "#838B8F", fill=NA),
        text = element_text(family = "Calibri"))


# plot Figure 1, S4 and S5 for soy production

figure_soy_production_wf <- function(y) {
  
  wf_map <- soy %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>% 
    mutate(soy_wf_rock = ifelse(soy_wf_rock == 0, NA, soy_wf_rock),
           wf_bin = case_when(
             is.na(soy_wf_rock) == T | soy_wf_rock == 0 ~ "0",
             soy_wf_rock > 0 & soy_wf_rock < 1000 ~ "0-1000",
             soy_wf_rock >= 1000 & soy_wf_rock < 2000 ~ "1000-2000",
             soy_wf_rock >= 2000 & soy_wf_rock < 3000 ~ "2000-3000",
             soy_wf_rock >= 3000 ~ "> 3000",
             TRUE ~ NA)) %>% 
    ggplot() +
    geom_sf(aes(fill = wf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_regions, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(~m^{3}* ~t^{-1},
                      na.value = "#f7f7f7",
                      limits = c("0", "0-1000", "1000-2000", "2000-3000", "> 3000"),
                      labels = c("0", "0-1000", "1000-2000", "2000-3000", "> 3000"),
                      values = c("#f7f7f7", "#92e5c2", "#00d29a", "#009b71", "#004d33")) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "A") + 
    theme_tufte() +
    theme(
      plot.title = element_text(size = 8),
      axis.text = element_text(size = 5), 
      axis.ticks = element_line(size = 0.1),
      legend.position = "inside",
      legend.position.inside = c(0.93,0.3), 
      legend.title = element_text(size = 5, margin = margin(b = 0.1, unit = 'cm'), hjust = -0.1), 
      legend.text = element_text(size = 5, margin = margin(l = 0.1, unit = 'cm')),
      legend.key.size = unit(0.15, "cm"),
      legend.key = element_rect(colour = 'black', linewidth = 0.1),
      text = element_text(family = "Calibri"))
  
  bwf_map <- soy %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    filter(is.na(year) == T | year == y) %>% 
    mutate(
      bwf_bin = case_when(
        is.na(soy_bwf_rock) == T | soy_bwf_rock == 0 ~ "0",
        soy_bwf_rock > 0 & soy_bwf_rock < 100 ~ "0-100",
        soy_bwf_rock >= 100 & soy_bwf_rock < 250 ~ "100-250",
        soy_bwf_rock >= 250 & soy_bwf_rock < 500 ~ "250-500",
        soy_bwf_rock >= 500 & soy_bwf_rock < 1000 ~ "500-1000",
        soy_bwf_rock >= 1000 ~ (">1000"),
        TRUE ~ NA)) %>% 
    ggplot() +
    geom_sf(aes(fill = bwf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_regions, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(~m^{3}* ~t^{-1},
                      na.value = "#f7f7f7",
                      breaks = c("0", "0-100", "100-250", "250-500", "500-1000", ">1000"),
                      values = c("#f7f7f7", "#b7daec", "#4aa2d0", "#376faa", "#194179", "#062d66")) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "B") +
    theme_tufte() +
    theme(
      plot.title = element_text(size = 8),
      axis.text = element_text(size = 5), 
      axis.ticks = element_line(size = 0.1), 
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.92,0.3), 
      legend.title = element_text(size = 5, margin = margin(b = 0.1, unit = 'cm'), hjust = -0.1),
      legend.text = element_text(size = 5, margin = margin(l = 0.1, unit = 'cm')),
      legend.key.size = unit(0.15, "cm"),
      legend.key = element_rect(colour = 'black', linewidth = 0.1),
      plot.margin = margin(r = 0.25, unit = 'cm'),
      text = element_text(family = "Calibri"))
  
  plot(p_combined <- (wf_map + inset_element(map_inset, left = 0.65, right = 1.15, bottom = 0.5, top = 1.1)) +
         bwf_map)
  
}

# export these maps as Fig. 1 (2017), S4 (2015) and S5 (2016)
# uncomment to export

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure1_soy_wf_2017.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_soy_production_wf("2017"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS5_soy_wf_2016.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_soy_production_wf("2016"),
#      dpi = 300)
# #
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS4_soy_wf_2015.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_soy_production_wf("2015"),
#      dpi = 300)


# map cattle production footprints ----------------------------------------

# plots Figure 2, S6 and S7 for cattle production (as living weight)

figure_cattle_production <- function(y) {
  
  cattle_wf <- cattle %>% 
    full_join(br %>% 
                  select(trase_id, geometry), by = "trase_id") %>% 
      filter(is.na(year) == T | year == y) %>% 
      mutate(
        wf_bin = case_when(
          is.na(tot_m3_per_tonlw_beef_dairy_anualpec_zanetti) == T | tot_m3_per_tonlw_beef_dairy_anualpec_zanetti == 0 ~ "0",
          tot_m3_per_tonlw_beef_dairy_anualpec_zanetti > 0 & tot_m3_per_tonlw_beef_dairy_anualpec_zanetti < 250 ~ "0-250",
          tot_m3_per_tonlw_beef_dairy_anualpec_zanetti >= 250 & tot_m3_per_tonlw_beef_dairy_anualpec_zanetti < 500 ~ "250-500",
          tot_m3_per_tonlw_beef_dairy_anualpec_zanetti >= 500 & tot_m3_per_tonlw_beef_dairy_anualpec_zanetti < 1000 ~ "500-1000",
          tot_m3_per_tonlw_beef_dairy_anualpec_zanetti >= 1000 ~ "> 1000",
          TRUE ~ NA)) %>% 
      ggplot() +
      geom_sf(aes(fill = wf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
      geom_sf(data = br_regions, fill = NA, size = 1.5, col = 'black') +
      scale_fill_manual(~m^{3}* ~(t ~LW)^{-1},
                        na.value = '#f7f7f7',                                       
                        breaks = c("0", "0-250", "250-500", "500-1000", "> 1000"),
                        labels = c("0", "0-250", "250-500", "500-1000", "> 1000"),
                        values = c("#f7f7f7", "#b7daec", "#4aa2d0", "#376faa", "#194179")) +
      ylim(-33.75077, 5.271841) + 
      xlim(-73.99045, -29.30279) +
      labs(title = "A") +
      theme_tufte() +
      theme(
        plot.title = element_text(size = 8),
        axis.text = element_text(size = 5), 
        axis.ticks = element_line(linewidth = 0.1), 
        legend.position = "inside",
        legend.position.inside = c(0.89,0.3), 
        legend.title = element_text(size = 5, margin = margin(b = 0.05, unit = 'cm'), hjust = -0.7),
        legend.text = element_text(size = 5, margin = margin(l = 0.1, unit = 'cm')),
        legend.key.size = unit(0.15, "cm"),
        legend.key = element_rect(colour = 'black', linewidth = 0.1),
        text = element_text(family = "Calibri"))
    
    pasture <- cattle %>% 
      full_join(br %>% 
                  select(trase_id, geometry), by = "trase_id") %>% 
      filter(is.na(year) == T | year == y) %>% 
      mutate(
        wf_bin = case_when(
          is.na(pasture_m3_per_tonlw_beef_dairy_anualpec) == T | pasture_m3_per_tonlw_beef_dairy_anualpec == 0 ~ "0",
          pasture_m3_per_tonlw_beef_dairy_anualpec > 0 & pasture_m3_per_tonlw_beef_dairy_anualpec < 1000 ~ "0-1000",
          pasture_m3_per_tonlw_beef_dairy_anualpec >= 1000 & pasture_m3_per_tonlw_beef_dairy_anualpec < 5000 ~ "1000-5000",
          pasture_m3_per_tonlw_beef_dairy_anualpec >= 5000 & pasture_m3_per_tonlw_beef_dairy_anualpec < 10000 ~ "5000-10,000",
          pasture_m3_per_tonlw_beef_dairy_anualpec >= 10000 & pasture_m3_per_tonlw_beef_dairy_anualpec < 20000 ~ "10,000-20,000",
          pasture_m3_per_tonlw_beef_dairy_anualpec >= 20000 ~"> 20,000",
          TRUE ~ NA)
      ) %>% 
      ggplot() +
      geom_sf(aes(fill = wf_bin, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
      geom_sf(data = br_regions, fill = NA, size = 1.5, col = 'black') +
      scale_fill_manual(~m^{3}* ~(t ~LW)^{-1},
                        na.value = "#f7f7f7",
                        limits = c("0", "0-1000", "1000-5000", "5000-10,000", "10,000-20,000", "> 20,000"),
                        labels = c("0", "0-1000", "1000-5000", "5000-10,000", "10,000-20,000", "> 20,000"),
                        values = c("#f7f7f7", "#92e5c2", "#00d29a", "#009b71", "#004d33", "#003125")) + 
      ylim(-33.75077, 5.271841) + 
      xlim(-73.99045, -29.30279) +
      labs(title = "B") +
      theme_tufte() +
      theme(
        plot.title = element_text(size = 8),
        axis.text = element_text(size = 5), 
        axis.ticks = element_line(linewidth = 0.1), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.94,0.3), #legend.position = c(0.89,0.3), 
        legend.title = element_text(size = 5, margin = margin(b = 0.05, unit = 'cm'), hjust = -0.1),
        legend.text = element_text(size = 5, margin = margin(l = 0.1, unit = 'cm')),
        legend.key.size = unit(0.15, "cm"),
        legend.key = element_rect(colour = 'black', linewidth = 0.1),   # replace by linewidth
        plot.margin = margin(r = 0.25, unit = 'cm'),
        text = element_text(family = "Calibri"))
    
    plot(cattle_wf + inset_element(map_inset, left = 0.65, right = 1.15, bottom = 0.5, top = 1.1) +
           pasture)
  
  
}

# export these maps as Fig. 2 (2017), S6 (2015) and S7 (2016)

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure2_cattle_wf_2017.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_cattle_production("2017"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS7_cattle_wf_2016.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_cattle_production("2016"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS6_cattle_wf_2015.png"),
#      width = 9,
#      height = 6,
#      units = "cm",
#      plot = figure_cattle_production("2015"),
#      dpi = 300)


# map soy and beef production benchmarks ----------------------------------

## soy production benchmarks 

## plot figure 5 (maps), S14, S15

## these graphs show panels A and B of above figures

## create bivariate classes

custom_pal_soy <- c(
  "1-1" = "#00b685",  #"< cutoff WF, no irr"
  "2-1" = "#4188bd",  #"< cutoff WF, irr"
  "1-2" = "#004d33",  #"> cutoff WF, no irr"
  "2-2" = "#194179")  #"> cutoff WF, irr"

legend_fig5 <- biscale::bi_legend(pal = custom_pal_soy,
                                  dim = 2,
                                  xlab = "Rainfed - Irrigated ",
                                  ylab = "below - above ",
                                  size = 8, 
                                  arrows = F) + 
  theme(text = element_text(size = 5, family = "Calibri"))

legend_plot_fig5 <- cowplot::ggdraw() + 
  cowplot::draw_plot(legend_fig5, width = 0.45, height = 0.45) 

plot_benchmark_soy <- function(y) {
  
  left <- soy_bench %>% 
    filter(year == y, 
           tag_def == "< cutoff def") %>%
    mutate(tag = paste0(tag_wf, ", ", tag_irr)) %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_basins, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(na.value = "white",
                      values = c("> cutoff WF, no irr" = "#004d33",
                                 "> cutoff WF, irr" = "#194179",
                                 "< cutoff WF, no irr" = "#00b685",
                                 "< cutoff WF, irr" = "#4188bd"), 
                      labels = c("> cutoff WF, no irr" = "Rainfed (above)",
                                 "> cutoff WF, irr" = "Irrigated (above)",
                                 "< cutoff WF, no irr" = "Rainfed (below)",
                                 "< cutoff WF, irr" = "Irrigated (below)"),
                      na.translate = F) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "A") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(linewidth = 0.1),
          legend.position = "none",
          text = element_text(family = "Calibri"))
  
  right <- soy_bench %>% 
    filter(year == y, 
           tag_def == "> cutoff def") %>%
    mutate(tag = paste0(tag_wf, ", ", tag_irr)) %>% 
    full_join(br %>% 
                select(trase_id, geometry), by = "trase_id") %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_basins, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(na.value = "white",
                      values = c("> cutoff WF, no irr" = "#004d33",
                                 "> cutoff WF, irr" = "#194179",
                                 "< cutoff WF, no irr" = "#00b685",
                                 "< cutoff WF, irr" = "#4188bd"), 
                      labels = c("> cutoff WF, no irr" = "Rainfed (above)",
                                 "> cutoff WF, irr" = "Irrigated (above)",
                                 "< cutoff WF, no irr" = "Rainfed (below)",
                                 "< cutoff WF, irr" = "Irrigated (below)"),
                      na.translate = F) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "B") +
    xlab("") +
    theme_tufte() +
    theme(plot.title = element_text(size = 12), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.ticks.x = element_line(linewidth = 0.1),
          legend.position = "none",
          plot.margin = margin(r = 0.15, unit = 'cm'),
          text = element_text(family = "Calibri"))
  
  
  plot_figure <- (left | right + plot_layout(guides = "collect")) | legend_plot_fig5   
  
  plot_figure
  
}

# export these maps as top panel of figure 5 (2017), S14 (2015), S15 (2016) 

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure5_soy_bench_2017_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_soy("2017"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS14_soy_bench_2015_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_soy("2015"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS15_soy_bench_2016_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_soy("2016"),
#      dpi = 300)


## cattle production benchmarks 

## plot figure 6 (maps), S16, S17

## these graphs show panels A and B of above figures

## create bivariate classes

custom_pal_cattle <- c(
  "1-1" = "#4aa2d0",  #"Below, lower impact"
  "2-1" = "#fec44f",  #"Below, higher impact"
  "1-2" = "#194179",  #"Above, lower impact"
  "2-2" = "#aa4512")  #"Above, higher impact"

legend_fig6 <- biscale::bi_legend(pal = custom_pal_cattle,
                         dim = 2,
                         xlab = "below - above ",
                         ylab = "lower - higher impact ",
                         size = 8, 
                         arrows = F) + 
  theme(text = element_text(size = 5, family = "Calibri"))

legend_plot_fig6 <- cowplot::ggdraw() + 
  cowplot::draw_plot(legend_fig6, width = 0.45, height = 0.45)

plot_benchmark_cattle <- function(y) {
  
  left <- cattle_bench %>% 
    filter(year == y, 
           tag_def == "< cutoff def") %>% 
    mutate(tag = paste0(tag_wf, ", ", tag_wsf)) %>% 
    full_join(br %>% 
                select(trase_id, geometry)) %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_basins, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(na.value = "white",
                      values = c("> cutoff WF, < cutoff WSF" = "#194179",
                                 "> cutoff WF, > cutoff WSF" = "#aa4512",
                                 "< cutoff WF, < cutoff WSF" = "#4aa2d0",
                                 "< cutoff WF, > cutoff WSF" = "#fec44f"),
                      labels = c("> cutoff WF, < cutoff WSF" = "Above, lower impact",
                                 "> cutoff WF, > cutoff WSF" = "Above, higher impact",
                                 "< cutoff WF, < cutoff WSF" = "Below, lower impact",
                                 "< cutoff WF, > cutoff WSF" = "Below, higher impact"),
                      na.translate = F) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "A",) +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 8), 
          axis.ticks = element_line(linewidth = 0.1),
          legend.position = "none", 
          text = element_text(family = "Calibri"))
  
  right <- cattle_bench %>% 
    filter(year == y, 
           tag_def == "> cutoff def") %>% 
    mutate(tag = paste0(tag_wf, ", ", tag_wsf)) %>% 
    full_join(br %>% 
                select(trase_id, geometry)) %>% 
    ggplot() +
    geom_sf(aes(fill = tag, geometry = geometry), col = NA, alpha = 0.75, show.legend = TRUE) +
    geom_sf(data = br_basins, fill = NA, size = 1.5, col = 'black') +
    scale_fill_manual(na.value = "white",
                      values = c("> cutoff WF, < cutoff WSF" = "#194179",
                                 "> cutoff WF, > cutoff WSF" = "#aa4512",
                                 "< cutoff WF, < cutoff WSF" = "#4aa2d0",
                                 "< cutoff WF, > cutoff WSF" = "#fec44f"),
                      labels = c("> cutoff WF, < cutoff WSF" = "Above, lower impact",
                                 "> cutoff WF, > cutoff WSF" = "Above, higher impact",
                                 "< cutoff WF, < cutoff WSF" = "Below, lower impact",
                                 "< cutoff WF, > cutoff WSF" = "Below, higher impact"),
                      na.translate = F) +
    ylim(-33.75077, 5.271841) + 
    xlim(-73.99045, -29.30279) +
    labs(title = "B",) +
    theme_tufte() +
    theme(plot.title = element_text(size = 12),
          axis.text.x = element_text(size = 8), 
          axis.ticks.x = element_line(linewidth = 0.1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          text = element_text(family = "Calibri"))
  
  plot_figure <- (left | right + plot_layout(guides = "collect")) | legend_plot_fig6
  
  plot_figure
  
}

# export these maps as top panel of figure 6 (2017), S16 (2015), S17 (2016) 

# ggsave(paste0(tmp, "_", Sys.Date(), "_figure6_cattle_bench_2017_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_cattle("2017"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS16_cattle_bench_2015_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_cattle("2015"),
#      dpi = 300)
# 
# ggsave(paste0(tmp, "_", Sys.Date(), "_figureS17_cattle_bench_2015_AB.png"),
#      width = 18,
#      height = 11,
#      units = "cm",
#      plot = plot_benchmark_cattle("2016"),
#      dpi = 300)
