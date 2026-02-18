# Details ----
#' help_dend.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-18
#' Content: 
#' + Pulled out from parent scripts (03_lhs_dend)
#' ++ Makes it easier to see what are plot settings (this script)
#' ++ Rather than actual stats (03_lhs_dend)
#' -----------

# starts with some functions to set up matrix----

make_mat <- function(df, by, meas, name = 'dend_mat'){
  
  dendtbl <- df %>% # bc PERMANOVA needs this instead...
    dplyr::group_by({{by}}, {{meas}}) %>% 
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = {{by}}, 
                values_from = count, 
                values_fill = 0)
  
  dend_mat <- dendtbl %>%
    dplyr::select(-by) %>%
    as.matrix()
  
  dend_meta <- dendtbl %>% # fine we'll take the names out
    dplyr::select(by)
  
  rownames(dend_meta) <- paste(dend_meta$x) 
}

lhs_meta <- make_mat(sitsdemo, Dbasin, LHS)

# check the base----

# then choose clusters----

# plotting help----