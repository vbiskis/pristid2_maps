# Details ----
#' src_mnglm.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' Content: 
#' + functions for autorun of mnglms
#' -----------

build_mn_models <- function(data, response, predictors, max_vif = 5, iterations = 20, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  all_iterations <- list()
  model_frequency <- list()
  model_rankings <- list()
  
  for (iter in 1:iterations) {
    message(paste("\n\n===== ITERATION", iter, "OF", iterations, "====="))
    
    if (!is.null(seed)) {
      iter_seed <- seed + iter - 1
      set.seed(iter_seed)
      message(paste("Using seed:", iter_seed))
    }
    
    shuffled_predictors <- sample(predictors)
    
    models <- list()
    results <- data.frame(
      formula = character(),
      aic = numeric(),
      max_vif = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Base 
    base_model <- mblogit(as.formula(paste(response, "~ 1")), data = data)
    models[[1]] <- base_model
    results[1, ] <- list(
      formula = paste(response, "~ 1"),
      aic = AIC(base_model),
      max_vif = NA,
      p_value = NA
    )
    
    # check forbidden combinations
    is_forbidden <- function(vars) {
      any(sapply(exclude_pairs, function(pair) all(pair %in% vars)))
    }
    
    is_continuous <- function(x) {
      is.numeric(data[[x]]) && length(unique(data[[x]])) > 10
    }
    
    # add a model to results
    add_model <- function(model, formula_str, base_for_lrt = base_model) {
      max_vif_val <- if(length(all.vars(formula(model))) > 2) max(car::vif(model)) else 1
      
      if(max_vif_val < max_vif) {
        lrt <- anova(base_for_lrt, model)
        p_val <- pchisq(lrt$Deviance[2], df = lrt$Df[2], lower.tail = FALSE)
        
        models[[length(models) + 1]] <<- model
        results[nrow(results) + 1, ] <<- list(
          formula = formula_str,
          aic = AIC(model),
          max_vif = max_vif_val,
          p_value = p_val
        )
        return(TRUE)
      }
      return(FALSE)
    }
    
    max_terms <- min(8, length(predictors))
    for(n_vars in 1:max_terms) {
      message(paste("\nTesting models with", n_vars, "terms"))
      combos <- combn(shuffled_predictors, n_vars, simplify = FALSE)
      
      # Randomize the order of combinations
      combos <- sample(combos)
      valid_combos <- combos[!sapply(combos, is_forbidden)]
      
      for(vars in valid_combos) {
        formula_str <- paste(response, "~", paste(vars, collapse = " + "))
        
        tryCatch({
          model <- mblogit(as.formula(formula_str), data = data)
          if(add_model(model, formula_str)) {
            # If model was added successfully, try interactions
            if(length(vars) > 1) {
              # Try all possible two-way interactions between variables
              pairs_indices <- combn(length(vars), 2, simplify = FALSE)
              
              # Randomize order of interaction pairs
              pairs_indices <- sample(pairs_indices)
              
              for(pair in pairs_indices) {
                var1 <- vars[pair[1]]
                var2 <- vars[pair[2]]
                
                # Skip if interaction pair is forbidden
                if(is_forbidden(c(var1, var2))) next
                
                # Only try interaction if at least one variable is continuous
                if((is_continuous(var1) && is_continuous(var2)) ||
                   (is_continuous(var1) != is_continuous(var2))) {
                  
                  int_formula <- paste(formula_str, "+", var1, "*", var2)
                  tryCatch({
                    int_model <- mblogit(as.formula(int_formula), data = data)
                    add_model(int_model, int_formula, model)
                  }, error = function(e) {
                    message(paste("Note: Failed to fit interaction:", int_formula))
                  })
                }
              }
            }
          }
        }, error = function(e) {
          message(paste("Note: Failed to fit model:", formula_str))
        })
      }
    }
    
    # Sort results by AIC
    results <- results[order(results$aic), ]
    row.names(results) <- NULL
    
    all_iterations[[iter]] <- list(
      models = models[order(results$aic)],
      results = results,
      predictors_order = shuffled_predictors
    )
    
    for (i in 1:nrow(results)) {
      formula_str <- results$formula[i]
      
      # Track model frequency
      if (formula_str %in% names(model_frequency)) {
        model_frequency[[formula_str]] <- model_frequency[[formula_str]] + 1
      } else {
        model_frequency[[formula_str]] <- 1
      }
      
      # Track model rankings
      if (formula_str %in% names(model_rankings)) {
        model_rankings[[formula_str]] <- c(model_rankings[[formula_str]], i)
      } else {
        model_rankings[[formula_str]] <- i
      }
    }
  }
  
  get_terms_key <- function(formula_str) {
    parts <- strsplit(formula_str, "~", fixed = TRUE)[[1]]
    if (trimws(parts[2]) == "1") return("intercept_only")
    
    terms <- unlist(strsplit(trimws(parts[2]), "\\+"))
    terms <- trimws(terms)
    return(paste(sort(terms), collapse = "|"))
  }
  
  # Group models by their terms
  formulas <- names(model_frequency)
  terms_keys <- sapply(formulas, get_terms_key)
  
  # Create a mapping from terms to formulas
  terms_to_formulas <- split(formulas, terms_keys)
  
  # Create consolidated model summary
  consolidated <- data.frame(
    terms_key = names(terms_to_formulas),
    frequency = sapply(terms_to_formulas, function(fs) sum(unlist(model_frequency[fs]))),
    stringsAsFactors = FALSE
  )
  
  # Add example formula
  consolidated$example_formula <- sapply(terms_to_formulas, function(fs) fs[1])
  
  # Calculate average AIC
  consolidated$avg_aic <- sapply(consolidated$terms_key, function(tk) {
    fs <- terms_to_formulas[[tk]]
    all_aics <- unlist(lapply(fs, function(f) {
      sapply(1:iterations, function(iter) {
        results_df <- all_iterations[[iter]]$results
        idx <- which(results_df$formula == f)
        if (length(idx) > 0) return(results_df$aic[idx[1]]) else return(NA)
      })
    }))
    return(mean(all_aics, na.rm = TRUE))
  })
  
  # Sort by frequency first, then by AIC
  consolidated <- consolidated[order(-consolidated$frequency, consolidated$avg_aic), ]
  row.names(consolidated) <- NULL
  
  # Find the most consistent top models
  top_5_pct <- quantile(consolidated$avg_aic, 0.05)
  consistent_top_models <- subset(consolidated, 
                                  frequency >= iterations/2 & 
                                    avg_aic <= quantile(consolidated$avg_aic, 0.05))
  
  return(list(
    iterations = all_iterations,
    model_summary = consolidated,
    consistent_top_models = consistent_top_models
  ))
}
