### Plots einzelne Funktionen 

df1 <- expand.grid(
  anonymity = seq(0, 1, 0.1), 
  MOD = c(1, 3, 5),
  cues = seq(0, 1, 0.5),
  base_resp = 0.8
)


## Identity Compartmentalization
IC_function <- function(anonymity) {
  comp <- (anonymity^2)/(anonymity^2+(1-anonymity)^2)
  return(comp)
}

df1$comp <- IC_function(df1$anonymity)



# Plot Identity Compartmentalization
ggplot(df1, aes(x = anonymity, y = comp)) +
  geom_line() +
  geom_point() +
  labs(title = "Relationship between Anonymity & Identity Compartmentalization",
       x = "Anonymity",
       y = "Compartmentalization") +
  theme_minimal()



## Felt Responsibility 
FR_function <- function(comp, base_resp) {
  feltresp <- base_resp * (1 - 0.8 * comp)^3
  return(feltresp)
}

df1$feltresp <- FR_function(df1$comp, df1$base_resp)

# Plot Felt Responsibility 
ggplot(df1, aes(x = comp, y = feltresp)) +
  geom_line() +
  geom_point() +
  labs(title = "Relationship between Identity Compartmentalization \nand Felt Responsibility",
       x = "Identity Compartmentalization",
       y ="Felt Responsibility") +
  theme_minimal()



# Concern about Impression on others 
CAI_function <- function(cues) {
  concern <- cues
  return(concern)
}

df1$concern <- CAI_function(df1$cues)

# Plot Concern about Impression on others 
ggplot(df1, aes(x = cues, y = concern)) +
  geom_line() +
  geom_point() +
  labs(title = "Relationship between Presence of Interpersonal Cues \nand Concern about Impression on Others",
       x = "Interpersonal Cues",
       y = "Concern about Impression on Others") +
  theme_minimal()



## Courage to express oneself 
CE_function <- function(concern) {
  courage <- concern * (-1) + 1
  return(courage)
}

df1$courage <- CE_function(df1$concern)

# Plot Courage to express oneself 
ggplot(df1, aes(x = concern, y = courage)) +
  geom_line() +
  geom_point() +
  labs(title = "Relationship between Concern about Impression on \nOthers and Courage to Express Oneself",
       x = "Concern about im Impression on Others",
       y = "Courage to Express Oneself") +
  theme_minimal()



