# Details ----
#' src_mncheck.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' Content: 
#' + code for multinomial diagnostic plots
#' -----------

#diagnostics----
plot_multinom_diag <- function(model) {
  
  probs <- fitted(model) #predicted probs from model
  
  y_levels <- colnames(probs) #match to response matrix
  resp_matrix <- matrix(0, nrow = nrow(probs), ncol = ncol(probs))
  colnames(resp_matrix) <- y_levels
  
  for(i in 1:nrow(probs)) { #and then fill it in
    actual_class <- which(y_levels == model$fitted.values[i])
    resp_matrix[i, actual_class] <- 1
  }
  
  #get residuals... plus add in some tiny numbers to help with zeros
  dev_resids <- sqrt(-2 * (resp_matrix * log(pmax(probs, 1e-10)) + 
                             (1-resp_matrix) * log(pmax(1-probs, 1e-10))))
  dev_resids <- dev_resids * ifelse(resp_matrix - probs < 0, -1, 1)
  
  plot_data <- data.frame(
    fitted = as.vector(probs),
    residuals = as.vector(dev_resids),
    category = rep(colnames(probs), each = nrow(probs))
  )
  
  plot_data <- plot_data[complete.cases(plot_data), ] # Remove any NA values
  
  old_par <- par(no.readonly = TRUE)
  par(mar = c(4, 4, 3, 4))
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  
  cols <- rainbow(length(unique(plot_data$category)))
  
  # 1. Residuals vs Fitted
  plot(plot_data$fitted, plot_data$residuals,
       xlab = "Fitted probabilities",
       ylab = "Deviance residuals",
       main = "Residuals vs Fitted",
       col = factor(plot_data$category),
       pch = 16,
       cex = 0.5)
  abline(h = 0, lty = 2)
  legend("bottomleft", 
         legend = unique(plot_data$category),
         col = cols, pch = 16, 
         cex = 0.8, inset = c(0.05, 0.05))
  
  # 2. Normal Q-Q plot
  qqnorm(plot_data$residuals, 
         main = "Normal Q-Q Plot",
         col = factor(plot_data$category),
         pch = 16,
         cex = 0.5)
  qqline(plot_data$residuals)
  
  # 3. Scale-Location plot
  plot(plot_data$fitted, sqrt(abs(plot_data$residuals)),
       xlab = "Fitted probabilities",
       ylab = "√|Deviance residuals|",
       main = "Scale-Location Plot",
       col = factor(plot_data$category),
       pch = 16,
       cex = 0.5)
  
  # 4. Histogram (instead of density plot)
  hist(plot_data$residuals, 
       main = "Histogram of Residuals",
       xlab = "Residuals",
       breaks = 30,
       col = "lightgray")
  
  par(old_par)
}

#predictions----
cv_multi <- function(model_formula, data, k = 5) {
  
  set.seed(123) #what does this even do?
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  
  accuracy <- numeric(k)
  class_error <- numeric(k)
  
  for(i in 1:k) {
    # Split 
    test_idx <- which(folds == i)
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]
    
    # Fit 
    cv_model <- nnet::multinom(model_formula, data = train_data, trace = FALSE)
    
    # Predict
    predictions <- predict(cv_model, newdata = test_data, type = "class")
    
    # Calculate 
    accuracy[i] <- mean(predictions == test_data$Species_NB)
    class_error[i] <- 1 - accuracy[i]
  }
  
  # Return 
  list(
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    mean_class_error = mean(class_error),
    fold_accuracies = accuracy
  )
}