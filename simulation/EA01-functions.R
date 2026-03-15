#' Compute identity compartmentalization from anonymity
#'
#' Identity compartmentalization is modeled as a non-linear transformation
#' of anonymity. The function maps anonymity values to a compartmentalization
#' score using a squared-ratio, yielding values between 0 and 1.
#'
#' @param anonymity The degree of anonymity, on a scale from 0 to 1.
#'
#' @return The identity compartmentalization score for the given anonymity values,
#'         on a scale from 0 to 1.

IC_function <- function(anonymity) {
  comp <- (anonymity^2)/(anonymity^2+(1-anonymity)^2)
  return(comp)
}


#' Felt responsibility as a function of identity compartmentalization
#'
#' Felt responsibility is modeled as a decreasing, non-linear function of
#' identity compartmentalization. Higher compartmentalization reduces felt
#' responsibility relative to a baseline level.
#'
#' @param comp The degree of identity compartmentalization, on a scale from 0 to 1.
#'            
#' @param base_resp The baseline felt responsibility when compartmentalization is zero,
#'                  on a scale from 0 to 1.
#'
#' @return The felt responsibility for the given parameters, on a scale from 0 to 1.

base_resp <- 0.8

FR_function <- function(comp, base_resp) {
  feltresp <- base_resp * (1 - 0.8 * comp)^3
  return(feltresp)
}



#' Calculate concern about impression on others from number of interpersonal cues
#'
#' Concern about impression on others is modeled as a direct function of the
#' number of interpersonal cues. Higher values of interpersonal cues correspond
#' to higher concern about impression on others.
#'
#' @param cues The number of interpersonal cues, on a scale from 0 to 1.
#'
#' @return The concern about impression on others for the given cues,
#'         on a scale from 0 to 1.

CAI_function <- function(cues) {
  concern <- cues
  return(concern)
}



#' Calculate courage to express oneself from concern about impression on others
#'
#' Courage to express oneself is modeled as an inverse function of concern about
#' impression on others. Higher concern corresponds to lower courage.
#'
#' @param concern The concern about impression on others, on a scale from 0 to 1.
#'
#' @return The courage to express oneself for the given concern values,
#'         on a scale from 0 to 1.

CE_function <- function(concern) {
  courage <- concern * (-1) + 1
  return(courage)
}



#' Calculate state disinhibition
#'
#' State disinhibition is modeled as a linear combination of measure of online disinhibition (MOD),
#' felt responsibility, and courage to express oneself, including an interaction term
#' between felt responsibility and courage to express oneself.
#'
#' @param feltresp Felt responsibility, on a scale from 0 to 1.
#'                 
#' @param courage Courage to express oneself, on a scale from 0 to 1.
#'                
#' @param MOD measure of online disinhibition (MOD), on a scale from 1 to 5.
#'            
#'
#' @return The state disinhibition score for the given parameters, on a scale from -0.1 to 1.2.

# State Disinhibition
SD_function <- function(feltresp, courage, MOD) {
  state_dis <- 0.2 * MOD - 0.3 * feltresp + 0.2 * courage - 0.1 * feltresp * courage
  # Transformation auf 0-1 Skala + leichter Noise
  state_dis <- (state_dis + 0.1)/1.3 + rnorm(length(state_dis), mean = 0, sd = 0.0)
  state_dis[state_dis > 1] <- 1
  state_dis[state_dis < 0] <- 0
  return(state_dis)
}
  



#' Calculate the degree of Self-Disclosure
#'
#' Transforms state disinhibition into expert-rated levels of self-disclosre
#'
#' @param anonymity The degree of anonymity, on a scale from 0 to 1.
#'                  
#' @param cues The number of interpersonal cues, on a scale from 0 to 1.
#'        
#' @param MOD measure of online disinhibition (MOD), on a scale from 1 to 5.
#'      
#' @param base_resp The baseline felt responsibility when compartmentalization is zero,
#'                  fixed to 0.8 based on heuristic considerations 
#'
#' @return observed level (low, medium, high) of self-disclosure in textual analyses
#'

# Self-Disclosure Kategorien
selfdis_function <- function(anonymity, cues, MOD, base_resp) {
  comp <- IC_function(anonymity)
  feltresp <- FR_function(comp, base_resp)
  concern <- CAI_function(cues)
  courage <- CE_function(concern)
  state_dis <- SD_function(feltresp, courage, MOD)
  
  self_disclosure <- cut(
    state_dis,
    breaks = c(0, 0.4, 0.6, 1),
    labels = c("no", "low", "high"),
    include.lowest = TRUE,
  )
  return(self_disclosure)
}



##### Datenframe erstellen
library(ggplot2)

df <- expand.grid(
  anonymity = seq(0, 1, 1), 
  MOD = c(1, 5),
  cues = seq(0, 1, 1),
  base_resp = 0.8
)


# Zwischenwerte berechnen
df$comp <- IC_function(df$anonymity)
df$feltresp <- FR_function(df$comp, df$base_resp)
df$concern <- CAI_function(df$cues)
df$courage <- CE_function(df$concern)
df$state_disinhibition <- SD_function(df$feltresp, df$courage, df$MOD)
df$self_disclosure <- selfdis_function(df$anonymity, df$cues, df$MOD, df$base_resp)



# Spalten sauber sortieren
df <- df[, c("anonymity", "feltresp", "cues", "courage", "MOD", "state_disinhibition", "self_disclosure")]



## PLOT STATE DISINHIBITION
ggplot(df, aes(x = anonymity, y = state_disinhibition, color = as.factor(cues))) +
  geom_point(size = 1.5) +
  geom_line(aes(group = cues), linewidth = 0.5) +
  facet_wrap(~MOD) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "0" = "grey",
      "1" = "black"
    )
  ) +
  labs(
    title = "State Disinhibition depending on Anonymity, Interpersonal Cues and MOD",
    x = "Anonymity",
    y = "State Disinhibition",
    color = "Interpersonal Cues"
  ) +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
  )



### PLOT SELF-DISCLOSURE
ggplot(df, aes(x = anonymity, y = cues, color = self_disclosure)) +
  geom_point(size = 5) +
  facet_wrap(~MOD) +
  scale_color_manual(
    values = c(
      "no" = "lightgray",
      "low" = "darkgray",
      "high" = "black"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Level of Self-Disclosure depending on Anonymity, Interpersonal Cues and MOD",
    x = "Anonymity",
    y = "Interpersonal Cues",
    color = "Expert Rating of Self-Disclosure"
  ) +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
  )




