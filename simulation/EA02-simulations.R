source("simulation/EA01-functions.R")
set.seed(4)

n = 1000

dfs <- expand.grid(
  id = 1:n,
  anonymity = c(0,0.5,1),
  cues = c(0,0.5,1)
)

dfs$MOD <- rbeta(nrow(dfs),4,4)*4+1
dfs$base_resp <- rbeta(nrow(dfs),4,1.5)

dfs$comp <- IC_function(dfs$anonymity)
dfs$feltresp <- FR_function(dfs$comp, dfs$base_resp)
dfs$concern <- CAI_function(dfs$cues)
dfs$courage <- CE_function(dfs$concern)
dfs$MOD_group <- cut(
  dfs$MOD,
  breaks = c(1,2.25,3.75,5),
  labels = c("low MOD", "medium MOD", "high MOD")
)

dfs$state_disinhibition <- SD_function(dfs$feltresp, dfs$courage, dfs$MOD)

dfs$self_disclosure <- selfdis_function(
  dfs$anonymity,
  dfs$cues,
  dfs$MOD,
  dfs$base_resp
)




library(dplyr)
library(ggplot2)



## PLOT STATE DISINHIBITION

ggplot(dfs, aes(x = anonymity, y = state_disinhibition, color = as.factor(cues))) +
  geom_jitter(size = 1.5, width = 0.05, height = 0.05, alpha = 0.5) +
  facet_wrap(~MOD_group) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
  ) +
  scale_color_manual(name = "Interpersonal Cues", values = c("lightgreen", "orange", "darkred")) +
  labs(title = "n = 10",
    x = "Anonymität",
    y = "State Disinhibition",
    color = "Interpersonal Cues") 



### PLOT SELF-DISCLOSURE

ggplot(dfs, aes(x = anonymity, y = cues, color = self_disclosure)) +
  geom_point(size = 5) +
  facet_wrap(~MOD_group) +
  scale_color_manual(values = c("lightgreen", "orange", "darkred")) +
  theme_minimal() +
  labs(x = "Anonymity", 
       y = "Interpersonal Cues") +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
  )


