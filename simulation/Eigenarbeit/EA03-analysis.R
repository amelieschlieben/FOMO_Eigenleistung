source("simulation/Eigenarbeit/EA02-simulations.R")

### Paket für ordinale logistische Regression installieren und laden
install.packages("MASS")
library("MASS")

dfs$self_disclosure <- factor(
  dfs$self_disclosure, 
  levels = c("no", "low", "high"),
  ordered = TRUE
)

model <- polr(self_disclosure ~ anonymity * cues,
              data = dfs,
              Hess = TRUE)


coefs <- coef(summary(model))
## Werte e^beta (Odds Ratio)
exp(coef(model))

## Konfidenzintervalle 
exp(confint(model))



# Einzeltests
z <- coefs[, "t value"]

p_values <- c(
  anonymity = pnorm(z["anonymity"], lower.tail = FALSE),
  cues = pnorm(z["cues"], lower.tail = TRUE),
  interaction = 2 * pnorm(abs(z["anonymity:cues"]), lower.tail = FALSE))

p_values


