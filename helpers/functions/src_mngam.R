# Details ----
#' src_mngam.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' Content: 
#' + functions for autorun of mngams
#' -----------

library(mgcv)
library(nlme)
library(gridExtra)
library(MuMIn)  
library(VGAM)
library(MASS) 

#check what k values are suited per variable
explore_mn_smooth <- function(data, variable, response = "Species_NB", k_values = c(3, 5, 8)) {
  
  plots <- list()
  
  # Raw data plot
  plots[["raw"]] <- ggplot(data, aes_string(x = variable, y = response)) +
    geom_jitter(alpha = 0.3, height = 0.2, width = 0) +
    theme_minimal() +
    ggtitle("Raw data")
  
  # Create a smooth plot for each k value
  for(k in k_values) {
    p <- ggplot(data, aes_string(x = variable)) + theme_minimal()
    
    for(sp in levels(data[[response]])) {
      tmp_data <- data
      tmp_data$y <- ifelse(tmp_data[[response]] == sp, 1, 0)
      
      form <- formula(paste("y ~ s(", variable, ", k=", k, ")"))
      m <- gam(form, family=binomial, data=tmp_data)
      
      new_x <- data.frame(x = seq(min(data[[variable]], na.rm=TRUE), 
                                  max(data[[variable]], na.rm=TRUE), 
                                  length.out=100))
      names(new_x) <- variable
      pred <- predict(m, newdata=new_x, type="response")
      
      p <- p + geom_line(data=data.frame(x=new_x[[1]], y=pred, species=sp),
                         aes(x=x, y=y, color=species))
    }
    
    p <- p + ggtitle(paste0("k = ", k)) +
      ylab("Probability") +
      theme(legend.position="right")
    
    plots[[paste0("k", k)]] <- p
  }
  
  return(plots)
}

#get model----
get_mngam <- function(data, k3_vars, k5_vars, k10_vars, 
                      threshold = 0.4, min_pred = 2, max_pred = 4) {
  
  # Get numeric variables for correlation check
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  # Only compute correlation matrix if there are numeric variables
  if(length(numeric_vars) > 1) {
    cor_matrix <- cor(data[numeric_vars], use = "complete.obs")
    
    too_high <- which(abs(cor_matrix) > threshold & upper.tri(cor_matrix), arr.ind = TRUE)
    excluded_pairs <- if(nrow(too_high) > 0) {
      paste(rownames(cor_matrix)[too_high[,1]], colnames(cor_matrix)[too_high[,2]], sep = ":")
    } else character(0)
  } else {
    excluded_pairs <- character(0)
  }
  
  # Remove response variable from predictors
  predictors <- setdiff(names(data), "Species_NB")
  
  # Initialize results dataframe
  results <- data.frame(
    predictors = character(),
    formula = character(),
    aic = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Function to create formula with appropriate smoothing terms
  create_formula <- function(vars) {
    terms <- sapply(vars, function(x) {
        if(is.factor(data[[x]]) || is.character(data[[x]])) {
          return(x)  # No smoothing for factors/characters
        } else {
          # Apply appropriate k value based on variable lists
          if(x %in% k3_vars) {
            return(paste0("sm.ps(", x, ", k = 3)"))
          } else if(x %in% k5_vars) {
            return(paste0("sm.ps(", x, ", k = 5)"))
          } else if(x %in% k10_vars) {
            return(paste0("sm.ps(", x, ", k = 10)"))
          } else {
            return(paste0("sm.ps(", x, ")"))  # default
          }
        }
    })
    
    paste("Species_NB ~", paste(terms, collapse = " + "))
  }
  
  # Check if necessary packages are loaded
  if(!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' is required but not loaded")
  }
  
  # Loop through predictor combinations
  for(n in min_pred:max_pred) {
    message(paste("Testing combinations with", n, "predictors"))
    combos <- combn(predictors, n, simplify = FALSE)
    
    for(pred_set in combos) {
      # Skip if predictors highly correlated 
      skip <- FALSE
      for(pair in excluded_pairs) {
        vars <- strsplit(pair, ":")[[1]]
        if(all(vars %in% pred_set)) {
          skip <- TRUE
          break
        }
      }
      if(skip) next
      
      formula <- create_formula(pred_set)
      
      message(paste("Trying formula:", formula))
      
      tryCatch({
        # Make sure we're using the correct function from VGAM
        model <- VGAM::vgam(as.formula(formula), family = VGAM::multinomial, data = data,
                            control = VGAM::vgam.control(epsilon = 1e-8, 
                                                         maxit = 200,
                                                         trace = FALSE))
        
        results <- rbind(results, data.frame(
          predictors = paste(pred_set, collapse = ", "),
          formula = formula,
          aic = AIC(model)
        ))
      }, error = function(e) {
        message(paste("Error with formula:", formula))
        message(e$message)
      })
    }
  }
  
  # Return results sorted by AIC
  if(nrow(results) > 0) {
    return(results[order(results$aic), ])
  } else {
    message("No successful models fitted")
    return(results)
  }
}
  