library(magrittr)
library(dplyr)

data("iris")

# cor_result_setosa <- iris %>%
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

# setosa: lowest Pearson cor. for Petal.Length vs Sepal.Width

# versicolor: lowest Pearson cor. for Sepal.Length vs Sepal.Width

# virginica: lowest Pearson cor. for Sepal.Length vs Petal.Width

# Setosa
cor.test(split.iris[[1]]$Petal.Length, split.iris[[1]]$Sepal.Width)

# Versicolor
cor.test(split.iris[[2]]$Sepal.Length, split.iris[[2]]$Sepal.Width)

# Virginica
cor.test(split.iris[[3]]$Sepal.Length, split.iris[[3]]$Petal.Width)
