library(ggplot2)
library(dplyr)
library(magrittr)

# Load data
data("mtcars")
# head(mtcars)

# Create separate data frame with only necessary values
df1 <- data.frame(
  power = mtcars$hp, 
  time = mtcars$qsec, 
  gears = mtcars$am, 
  mpg = mtcars$mpg
)

# Add column with gearbox labels and column with fuel consumption in liters/100 km
df1 %<>% mutate(
  gears.label = case_when(
    gears == 0 ~ "automat", 
    gears == 1 ~ "manual"
  ),
  lp100km = 235.215 / mpg
)

# Create plot
g <- ggplot(df1, aes(x = power, y = time))
g + 
  geom_point(aes(color = gears.label, size = lp100km), shape = 19) +
  labs(
    x = "power [HP]",
    y = "1/4 mile time [s]",
    title = "Data from Motor Trend US magazine (1974)",
    color = "gears",
    size = "fuel"
  ) +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

# Save plot to .pdf file
ggsave("a4_plot.pdf", device = "pdf", width = 15, height = 10, units = "cm")
  
