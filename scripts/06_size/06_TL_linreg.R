# Details ----
#' 06_TL_linreg.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2024 Oct 2024
#' Content: 
#' TL trends ~ time
#' -----------

source('helpers/help_plot.R')
source('helpers/help_stats.R')
source('helpers/functions/src_summfxn.R')

sitsiz <- read_xlsx('data/processed/sitsiz.xlsx')

sitsiz <- sitsiz %>% 
  filter(Species_NB != 'Pristis sp.',
         !is.na(Size_Final))

sitsiz$Species_NB <- as.factor(sitsiz$Species_NB)

#Regression----
spec_reg_models <- lapply(split(sitsiz, sitsiz$Species_NB), 
                          function(specdata) {
                            lm(logTL2 ~ Cap_Year, data = specdata)
                          })

# View equations
print(spec_reg_models)
par(mfrow=c(2,2))
plot(spec_reg_models[[1]])
plot(spec_reg_models[[2]])
plot(spec_reg_models[[3]])
plot(spec_reg_models[[4]])
#hmm maybe log TL

#okay! all neg... let's see what's sig

get_equation <- function(model) {
  eq <- as.character(round(coef(model)[1], 2)) # coefficient for intercept
  eq <- paste0("y = ", eq)
  eq <- paste0(eq, " + ")
  eq <- paste0(eq, round(coef(model)[2], 2), "x") # coefficient for Cap_Year
  return(eq)
}
equations <- sapply(spec_reg_models, get_equation)

#let's also get p and r2
p_values <- sapply(spec_reg_models, function(model) {
  coef(summary(model))[, 4][2] 
})

r_squared_values <- sapply(spec_reg_models, function(model) {
  summary(model)$r.squared
})

regression_table <- data.frame(
  Species_NB = levels(sitsiz$Species_NB),
  Equation = equations,
  P_value = p_values,
  R_squared = r_squared_values
)

print(regression_table)
