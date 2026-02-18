# Details ----
#' src_predplot.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' - pulled out function for own call, 2025-12-23
#' Content: 
#' get predicted residuals to build mn plot
#' -----------

# Function to create prediction data for one variable
create_pred_data <- function(var_name, var_values, df, model) {
  # Create newdata with varying variable and others at median
  newdata <- data.frame(
    transout = if(var_name == "transout") var_values else median(df$transout),
    Tmax = if(var_name == "Tmax") var_values else median(df$Tmax),
    logPrec = if(var_name == "logPrec") var_values else median(df$logPrec),
    STRM_ORDER = if(var_name == "STRM_ORDER") var_values else median(df$STRM_ORDER),
    logRF = if(var_name == "logRF") var_values else median(df$logRF),
    logDB = if(var_name == "logDB") var_values else median(df$logDB)
  )
  
  # Get predictions
  preds <- predict(model, newdata = newdata, type = "response")
  n_species <- ncol(preds)
  
  # Format for ggplot
  pred_df <- data.frame(
    variable_value = var_values,
    variable_name = var_name,
    preds
  ) %>%
    setNames(c("variable_value", "variable_name", paste("Species", 0:(n_species-1)))) %>%
    pivot_longer(cols = starts_with("Species"), 
                 names_to = "Species", 
                 values_to = "Probability")
  
  return(pred_df)
}
