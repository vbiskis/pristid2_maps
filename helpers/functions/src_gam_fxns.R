# Details ----
#' src_gam_fxns.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-24
#' Content: 
#' 3 x functions for setting and comparing gams
#' for repetitive steps!
#' -----------

# fit and compare models ----
fit_gam <- function(data, formulas, family = gaussian, method = "REML", select = TRUE) {
  
  results <- map(names(formulas), function(name) {
    cat(sprintf("\nFitting %s...", name))
    
    mod <- tryCatch({
      gam(formulas[[name]], 
          family = family, 
          data = data, 
          method = method,
          select = select)
    }, error = function(e) {
      message(sprintf("  FAILED: %s", e$message))
      return(NULL)
    })
    
    if (is.null(mod)) return(NULL)
    
    # Extract diagnostics
    list(
      name = name,
      model = mod,
      dev_explained = summary(mod)$dev.expl * 100,
      aic = AIC(mod),
      bic = BIC(mod),
      n_params = sum(mod$edf)
    )
  })
  
  # Remove failed models
  results <- compact(results)
  
  # Create comparison table
  comparison <- map_df(results, ~tibble(
    model = .x$name,
    dev_expl = .x$dev_explained,
    AIC = .x$aic,
    BIC = .x$bic,
    n_params = .x$n_params
  )) %>%
    arrange(BIC)
  
  cat("\n\n=== MODEL COMPARISON ===\n")
  print(comparison, n = Inf)
  
  return(list(models = results, comparison = comparison))
}

# diagnostic checker ----
check_model <- function(model, name = "") {
  cat(sprintf("\n=== %s ===\n", name))
  print(summary(model))
  cat("\nResidual plots:\n")
  appraise(model)
  cat("\nPerformance:\n")
  print(performance(model))
}

# Pairwise ANOVA ----
compare_nested <- function(model_results, pairs) {
  # Extract models into a named list first
  models <- setNames(
    map(model_results, "model"),
    map_chr(model_results, "name")
  )
  
  # pairs should be list of c("simpler", "complex")
  results <- map_df(pairs, function(pair) {
    m1 <- models[[pair[1]]]
    m2 <- models[[pair[2]]]
    
    if (is.null(m1) || is.null(m2)) {
      return(tibble(simpler = pair[1], complex = pair[2], 
                    p_value = NA, sig = "FAILED"))
    }
    
    test <- anova(m1, m2, test = "Chisq")
    
    tibble(
      simpler = pair[1],
      complex = pair[2],
      p_value = test$`Pr(>Chi)`[2],
      sig = ifelse(p_value < 0.05, "***", 
                   ifelse(p_value < 0.1, "*", "ns"))
    )
  })
    return(results)
}
