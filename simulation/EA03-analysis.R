source("simulation/EA02-simulations.R")

### Paket für ordinale logistische Regression installieren und laden
install.packages("MASS")
library("MASS")

dfs$an_self_dis <- factor(
  dfs$self_disclosure, 
  levels = c("no", "low", "high"),
  ordered = TRUE
)

model <- polr(an_self_dis ~ anonymity * cues,
              data = dfs,
              Hess = TRUE)

summary(model)
exp(coef(model))
exp(confint(model))
