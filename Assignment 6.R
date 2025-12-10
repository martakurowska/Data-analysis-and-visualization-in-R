library(magrittr)
library(dplyr)
library(ggplot2)

# TASK 1

data("iris")

# cor_result_setosa <- ggplot2>%
#   filter(Species == "setosa") %>%
#   select(1:4) %>%
#   cor(.) 
# 
# print(cor_result_setosa)
# 
# cor_result_versicolor <- iris %>%
#   filter(Species == "versicolor") %>%
#   select(1:4) %>%
#   cor(.)
# 
# print(cor_result_versicolor)
# 
# cor_result_virginica <- iris %>%
#   filter(Species == "virginica") %>%
#   select(1:4) %>%
#   cor(.)
# 
# print(cor_result_virginica)


split.iris <- iris %>%
  group_by(Species) %>%
  group_split() %>%
  lapply(function(df) df[, 1:4])


cor_results <- lapply(split.iris, cor)
print(cor_results)

# TASK 2

# Setosa: lowest Pearson cor. for Petal.Length vs Sepal.Width
# Versicolor: lowest Pearson cor. for Sepal.Length vs Sepal.Width
# Virginica: lowest Pearson cor. for Sepal.Length vs Petal.Width

# Setosa
cor.test(split.iris[[1]]$Petal.Length, split.iris[[1]]$Sepal.Width)

# Versicolor
cor.test(split.iris[[2]]$Sepal.Length, split.iris[[2]]$Sepal.Width)

# Virginica
cor.test(split.iris[[3]]$Sepal.Length, split.iris[[3]]$Petal.Width)

# Setosa: p-value=0.217, very weak correlation, statistically insignificant.
# Versicolor: p-value=8.772e-05, strong correlation, statistically significant.
# Virginica: p-value=0.04798, weak correlation, at the edge of significance.


# TASK 3

data <- read.table("wig20.dat", header = FALSE)
data.ts <- ts(data$V2, frequency = 4)

# I picked a minute as a unit of time, so as the samples were taken with a 15s
# interval, the frequency should be equal to 4
# Using a minute-based frequency allows us to capture high-resolution dynamics 
# in the data


# TASK 4

data.comp <- decompose(data.ts)
# plot(data.comp)

acf.ts <- acf(data.comp$x, lag.max = 40, plot = FALSE)
acf.rand <- acf(na.omit(data.comp$random), lag.max = 40, plot = FALSE)

acf.df <- data.frame(lag = acf.ts$lag, ts = acf.ts$acf, rand = acf.rand$acf)


# TASK 5

ggplot(acf.df, aes(x = lag)) + 
  geom_point(aes(y = ts, color = "Original"), size = 3) + 
  geom_point(aes(y = rand, color = "Random"), size = 3) +
  labs(x = "lag", y = "acf", color = "series") +
  theme_bw()

