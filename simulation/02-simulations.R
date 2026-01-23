source("simulation/01-functions.R")

n = 100
df <- expand.grid(
  id = 1:n,
  anonymity = c(0, 0.5, 1),
  cues = c(0, 0.5, 1)
)
df$MOD <- rbeta(nrow(df), 4, 4)*4 + 1

df$base_resp <- rbeta(nrow(df), 4, 1.5)

df$bad_sentence_percentage <- curse_function(df$anonymity, df$cues, df$MOD, df$base_resp)


ggplot(df, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, position_dodge=0.1)
