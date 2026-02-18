# Details ----
#' 04_mngam_4_plot.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' - pulled out function for own call, 2025-12-23
#' Content: 
#' + produce fig 9 model output (c-f)
#' -----------

source('helpers/functions/src_predplot.R')
source('helpers/help_stats.R')
source('helpers/help_plot.R')
theme_set(mytheme)
library(performance)
library(mclogit)

# get dat----
gammy <- readxl::read_xlsx("data/processed/gammy.xlsx") 
gammy$Species_NB <- as.numeric(factor(gammy$Species_NB)) - 1 #need it as 0 to 3

# pred vars----
var_names <- c("transout", "Tmax", "logPrec", 
               "STRM_ORDER", # when no DB
                "logDB", # when no clavata
                "logRF" # not sig on its own
               )

# get model----
# mod5 for all spec, mn_nc_6 no clavata
gam_mod5 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5)),
                family = multinom(K=3),
                data = gammy)

# apply fxn----

all_pred_data <- map_dfr(var_names, function(var) {
  var_range <- seq(min(gammy[[var]]), max(gammy[[var]]), length = 100)
  create_pred_data(var, var_range, gammy, gam_mod5)
})

# make plot list----
plot_list <- lapply(1:length(var_names), function(i) {
  var <- var_names[i]
  plot_data <- filter(all_pred_data, variable_name == var)
  
  p <- ggplot(plot_data, aes(x = variable_value, y = Probability, color = Species)) +
    geom_line(size = 0.8, alpha = 0.8) +
    ylim(0, 1) +
    scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen"),
                       labels = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")) +
    labs(x = case_when(
      var == "transout" ~ "Distance from Outlet (√km)",
      var == "Tmax" ~ "Max Monthly Temp. (°C)",
      var == "logPrec" ~ "Precipitation (ln mm/month)",  
      var == "STRM_ORDER" ~ "Stream Order",
      var == "logDB" ~ "Drainage Basin Area (log₁₀km²)",
      var == "logRF" ~ "Distance from Boating Facility (log-km)"
    ),
    y = "Probability of Species Encounter") +
    theme(
      panel.spacing.x = unit(0.5, "lines"),
      plot.margin = margin(l = 5, t = 2, b = 2, unit = "pt"),
      legend.text = element_text(size = 11, face = "italic"),
      legend.position = "bottom"
    ) 
  
  if (i %in% 1:4) { #dear god no legend for these guys
    p <- p + theme(legend.position = "none")
  }
  
  if (i %in% 2:4) { #get rid of y axis text for inner graphs
    p <- p + labs(y = "")
  } else {
    p <- p + labs(y = "Probability of Encounter")
  }
  
  if (i %in% c(2, 4)) {
    p <- p + theme(axis.text.y = element_blank())
  } else {
    p <- p 
  }
  
  if (i %in% 3:4) { #lol we gettin picky now
    p <- p + ylim(0, 0.8)
  } else {
    p <- p 
  }
  
  if (i %in% 3) { #oh you thought I was done
    p <- p + labs(x = expression("Precipitation ("*italic("ln")*" mm·mo"^{-1}*")"))
  } else {
    p <- p 
  }
  
  return(p)
})

# save plots----

saveRDS(plot_list, 'data/rds/spec_gam_plots.RDS')
