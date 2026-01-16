df <- expand.grid(
  CE = c(0, 0.5, 1),
  FR = c(0, 0.5, 1),
  MOD = c(1, 3, 5)
)

b0 <- 0
b1 <- 0.2
b2 <- -0.3
b3 <- 0.2
b4 <- -0.1

df$SOD <- b0 + b1*df$MOD + b2*df$FR + b3*df$CE + b4*df$FR*df$CE


library(ggplot2)

ggplot(df, aes(x = FR, y = SOD, color = factor(CE), group = CE)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ MOD, labeller = label_both) +
  labs(
    x = "FR",
    y = "SOD",
    color = "CE",
    title = "Einfluss von FR auf SOD",
    subtitle = "Parallele Linien für CE, getrennt nach MOD"
  ) +
  theme_minimal(base_size = 13)
