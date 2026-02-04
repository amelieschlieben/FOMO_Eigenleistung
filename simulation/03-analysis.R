source("simulation/02-simulations.R")

install.packages("multcomp")
library(multcomp)


## Wir haben einen fit erstellt
fit_lm <- lm(bad_sentence_percentage ~ anonymity * cues, data=df)

## Nullhypothesen definieren (Richtung kann man nicht mischen, deshalb getrennt)
hyps_less <- c("anonymity <=0", "anonymity:cues <= 0")
hyp_greater <- c("cues >= 0")

## glht Objekt erstellen 
# für Nullhypothesen kleiner gleich
fit_glht_less <- glht(fit_lm, hyps_less)
summary(fit_glht_less)

# für Nullhypothesen größer gleich
fit_glht_greater <- glht(fit_lm, hyp_greater)
summary(fit_glht_greater)


summary(fit_lm)
