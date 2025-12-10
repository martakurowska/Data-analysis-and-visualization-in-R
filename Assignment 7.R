library(magrittr)
library(dplyr)
library(ggplot2)

# TASK 1

data("ChickWeight")
split.lm <- ChickWeight %>%
  group_by(Diet) %>%
  group_split() %>% 
  lapply(function(df) summary(lm(df$weight ~ df$Time)))

print(split.lm)
# Diet 3 gives the greatest weight gain, as it has the greatest slope

# TASK 2 

ggplot(ChickWeight, aes(Time, weight, color = Diet, fill = Diet)) +
  geom_point(alpha=0.3, size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x, alpha = 0.25) +
  theme_bw()

# TASK 3 

# Load data
orings <- read.table("orings.txt", header = TRUE)
orings$tempC <- (orings$temp - 32) * 5/9 
ggplot(orings,aes(tempC, fail)) + 
  geom_point()  +
  labs(
    x = "Temperature [C]",
    y = "O-ring failure"
  ) +
  theme_bw()

# TASK 4
oring.lr <- glm(formula = fail ~ tempC, family = binomial, data = orings)
summary(oring.lr)

# model is statistically significant at a=0.05 because the temperature coeff.
# has a p-value of 0.032, which is below 0.05

OR <- exp(coef(oring.lr)["tempC"] * (10 - 15))
OR

# odds of failure increase around 8 times

# TASK 5
newTempC <- data.frame(tempC = seq(1, 30, 1))
newTempC$fail <- predict(oring.lr, newTempC, type = "response")

orings_filtered <- orings %>%
  filter(tempC < 30)

ggplot(orings_filtered, aes(tempC, fail)) + 
  geom_point()  +
  labs(
    x = "Temperature [C]",
    y = "O-ring failure"
  ) +
  geom_line(data = newTempC, aes(tempC, fail), color = "red") +
  theme_bw()

