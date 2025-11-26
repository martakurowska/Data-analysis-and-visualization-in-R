library(ggplot2)
library(MASS)

data <- read.table("data05.txt", header=FALSE, col.names = "x")

g <- ggplot(data)
hist <- geom_histogram(
    aes(x = x, y = after_stat(density)), 
    fill="blue", 
    colour="black", 
    bins = 20,
    alpha=0.5
  )
g + hist

# Presented histogram is not symmetrical, it possesses long right tail, therefore data is neither 
# from a Gaussian distribution nor Cauchy distribution. As the values are in <0;1> range the Gamma
# distribution is also unlikely.
# Conclusion: Data probably represents a Beta distribution

shapiro.test(data$x)

# p-value << 0.05 so therefore we reject the hypothesis that the data comes from 
# a normal (Gaussian) distribution

fit.cauchy <- fitdistr(data$x, "cauchy")
fit.cauchy$loglik

fit.normal   <- fitdistr(data$x, "normal")
fit.normal$loglik

# Calculating starting points for Beta distribution form MOM formula
m <- mean(data$x)
v  <- var(data$x)

a <- m * ((m * (1 - m)) / v - 1)
b <- (1 - m) * ((m * (1 - m)) / v - 1)

fit.beta   <- fitdistr(data$x, "beta", start=list(shape1=a, shape2=b))
fit.beta$loglik

fit.gamma  <- fitdistr(data$x, "gamma")
fit.gamma$loglik

# The higher loglik the higher likelihood of data coming from the tested distribution
# Here the maximal LL value is obtained for a Beta distribution, therefor it is the most probable

ks.test(data$x, "pcauchy",
        location = fit.cauchy$estimate["location"],
        scale    = fit.cauchy$estimate["scale"])

ks.test(data$x, "pnorm", 
        mean = fit.normal$estimate["mean"], 
        sd = fit.normal$estimate["sd"])

ks.test(data$x, "pbeta",
        shape1 = fit.beta$estimate["shape1"],
        shape2 = fit.beta$estimate["shape2"])

ks.test(data$x, "pgamma",
        shape = fit.gamma$estimate["shape"],
        rate  = fit.gamma$estimate["rate"])

# Only Beta distribution has p-value > 0.05, therefore it is the only hypothesis we cannot reject

g + hist +
  stat_function(
    fun = dbeta,
    args = as.list(fit.beta$estimate),
    colour="red",
    size=1.5
  )
