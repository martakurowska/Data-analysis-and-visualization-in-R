library(magrittr)
library(dplyr)
library(tidyr)

# Task 1
data1 <- read.table("er_N1000_k8_O0.1_part1.txt", header=TRUE)
head(data1)

# Task 2
data2 <- data1 %>% 
  group_by(rate, observers, method) %>%
  summarise(prec.mean=mean(precision),
            css0.95=quantile(rank, probs=0.95),
            time.mean=mean(time))
data2

# Task 3
data3 <- data2 %>%
  filter(observers=="BC") %>%
  group_by(rate) %>%
  summarise(method.worst=method[which.min(prec.mean)])
data3

# Task 4
data2_wide <- data2 %>%
  select(rate, observers, method, prec.mean) %>%
  pivot_wider(names_from = method, 
              values_from = prec.mean)
data2_wide

# Task 5
result <- data2_wide %>% filter(observers=="Random")
result


