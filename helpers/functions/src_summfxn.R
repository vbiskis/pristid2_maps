# Details ----
#' src_dist.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Split to two scripts 2025-12-10
#' Content: 
#' + function to visualise data
#' -----------

#Load load load----

pacman::p_load('dplyr', 'tidyverse', 
               'ggplot2', 'patchwork')

# Shiny new funx----
sit_sum <- function(dat, by, var){
  dat %>% 
    dplyr::group_by({{by}}) %>% 
    dplyr::summarise(med = median({{var}}, na.rm = T))}

# Plus a function to viz data----
sf_sum_box <- function(dat, x, y, z = NULL, 
                       met = median,
                       y_lab = y, 
                       x_lab = x,
                       scale = FALSE){
  
  if (is.null(z)){ 
    dat_wide <- dat %>% 
      dplyr::group_by(.data[[x]]) %>% 
      dplyr::summarise(y_met = round(met(.data[[y]], na.rm = T), 2)) %>% 
      tidyr::pivot_wider(names_from = .data[[x]], values_from = y_met)
  } else{
    dat_wide <- dat %>% 
      dplyr::group_by(.data[[x]], .data[[z]]) %>% 
      dplyr::summarise(y_met = round(met(.data[[y]], na.rm = T), 2)) %>% 
      tidyr::pivot_wider(names_from = .data[[x]], values_from = y_met)}
  
  gg <- dat %>% 
    ggplot() +
    aes(x = .data[[y]], y = .data[[x]]) +
    xlab(x_lab) +
    ylab(y_lab) +
    geom_boxplot() +
    theme_classic()
  
  if (!is.null(z)){
    gg <- gg + aes(colour = .data[[z]])
  }
  
  if (scale == "log10"){
    gg <- gg + scale_x_log10() 
  } else {
    gg <- gg + scale_x_continuous()
  }
  
  patchwork::wrap_plots(
    gridExtra::tableGrob(dat_wide, rows = NULL, theme = gridExtra::ttheme_default(base_size = 12)),
    gg, 
    ncol = 1)
}


