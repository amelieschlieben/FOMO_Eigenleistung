source("simulation/01-functions.R")
set.seed(4)

n = 95
df <- expand.grid(
  id = 1:n,
  anonymity = c(0, 0.5, 1),
  cues = c(0, 0.5, 1)
)
df$MOD <- rbeta(nrow(df), 4, 4)*4 + 1

df$base_resp <- rbeta(nrow(df), 4, 1.5)

df$bad_sentence_percentage <- curse_function(df$anonymity, df$cues, df$MOD, df$base_resp)


p1 = ggplot(df, aes(x= anonymity, y = bad_sentence_percentage, color = factor(cues))) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line",
               position = position_dodge(width = 0.1))

print(p1)

df
