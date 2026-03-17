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

# Überprüfung mit Vektor
an <- c(0.2, 0.5, 0.8)
IC_function(an)

base_resp <- 0.8

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

FR_function <- function(comp, base_resp) {
  feltresp <- base_resp * (1 - 0.8 * comp)^3
  return(feltresp)
}


# Überprüfung mit Vektor
FR_function(an,base_resp = 0.8)


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


# Überprüfung mit Vektor
CAI_function(an)


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

# Überprüfung mit Vektor 
CE_function(an)

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

SD_function <- function(feltresp, courage, MOD) {
  state_dis <- 0.2 * MOD + (-0.3) * feltresp + 0.2 * courage + (-0.1) * feltresp * courage
  return(state_dis)
}

#' Calculate the percentage of sentences containing at least one curse word
#'
#' Transforms state disinhibition into an observed outcome with added
#' noise and bounds the result to the interval [0, 1].
#'
#' @param anonymity The degree of anonymity, on a scale from 0 to 1.
#'                  
#' @param cues The number of interpersonal cues, on a scale from 0 to 1.
#'        
#' @param MOD measure of online disinhibition (MOD), on a scale from 1 to 5.
#'      
#' @param base_resp The baseline felt responsibility when compartmentalization is zero,
#'                  on a scale from 0 to 1.
#'
#' @return The percentage of sentences containing at least one curse word for the given
#'         parameters, as values on a scale from 0 to 1.

curse_function <- function(anonymity, cues, MOD, base_resp) {
  comp <- IC_function(anonymity)
  feltresp <- FR_function(comp, base_resp)
  concern <- CAI_function(cues)
  courage <- CE_function(concern)
  state_dis <- SD_function(feltresp, courage, MOD)
  bad_sentence_percentage <- (state_dis + 0.1)/1.3 + rnorm(length(state_dis), mean = 0, sd = 0.0)
  bad_sentence_percentage[bad_sentence_percentage > 1] <- 1
  bad_sentence_percentage[bad_sentence_percentage < 0] <- 0
  return(bad_sentence_percentage)
}

library(ggplot2)
df <- expand.grid(
  anonymity = c(0, 0.5, 1),
  MOD = c(1, 3, 5),
  cues = c(0, 0.5, 1),
  base_resp = c(0.5, 0.9)
)

df$bad_sentence_percentage <- curse_function(df$anonymity, df$cues, df$MOD, df$base_resp)

ggplot(df, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  facet_grid(MOD ~ base_resp) +
  geom_point() + 
  geom_line()




