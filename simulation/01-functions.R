##### Dissociative Anonymity

### Anonymity auf Identity Compartmentalization 
IC_function <- function(anonymity) {
  comp <- (anonymity^2)/(anonymity^2+(1-anonymity)^2)
  return(comp)
}

# Überprüfung mit Vektor
an <- c(0.2, 0.5, 0.8)
IC_function(an)


### Identity Compartmentalization auf Felt Responsibility 
FR_function <- function(comp, base_resp) {
  feltresp <- base_resp *(1 - 0.8*comp)^3
  return(feltresp)
}

# Überprüfung mit Vektor
FR_function(an)





###### Invisibility
CAI_function <- function(cues) {
  concern <- cues
  return(concern)
}

# Überprüfung mit Vektor
CAI_function(an)


CE_function <- function(concern) {
  courage <- concern * (-1) +1
  return(courage)
}


# Überprüfung mit Vektor 
CE_function(an)




##### Kernfunktion 
SD_function <- function(feltresp, courage, MOD) {
  state_dis <- 0.2 * MOD + (-0.3) * feltresp + 0.2 * courage + (-0.1) * feltresp * courage
  return(state_dis)
}


##### Combine atomic functions into one “super function”
curse_function <- function(anonymity, cues, MOD, base_resp) {
  comp <- IC_function(anonymity)
  feltresp <- FR_function(comp, base_resp)
  concern <- CAI_function(cues)
  courage <- CE_function(concern)
  state_dis <- SD_function(feltresp, courage, MOD)
  bad_sentence_percentage <- (state_dis + 0.1)/1.3 + rnorm(mean = 0, sd = 0.1)
  bad_sentence_percentage [bad_sentence_percentage > 1] <- 1
  bad_sentence_percentage [bad_sentence_percentage < 0] <- 0
  return(bad_sentence_percentage)
}


library(ggplot2)

ggplot(df, aes(x = feltresp, y = state_dis, color = factor(courage), group = courage)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ MOD, labeller = label_both) +
  labs(
    x = "feltresp",
    y = "state_dis",
    color = "courage",
    title = "Einfluss von feltresp auf state dis",
    subtitle = "Parallele Linien für courage, getrennt nach MOD"
  ) +
  theme_minimal(base_size = 13)




