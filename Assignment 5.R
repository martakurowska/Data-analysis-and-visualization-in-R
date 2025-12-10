library(ggplot2)
library(MASS)

# Load data
data <- read.table("data05.txt", header = FALSE, col.names = "x")


# Plot histogram
g <- ggplot(data)
hist <- geom_histogram(
    aes(x = x, y = after_stat(density)), 
    fill="blue", 
    colour="black", 
    bins = 20,
    alpha=0.5
  )
g + hist

# Presented histogram is not symmetrical, it possesses long right tail, 
# therefore data is neither from a Gaussian nor from a Cauchy distribution. 
# As the values are in <0;1> range with maximum at approx. 0.3-0.35 the Gamma 
# distribution is also unlikely.
# Conclusion: Data probably represents a Beta distribution


# Shapiro-Wilk normality test
shapiro.test(data$x)

# p-value << 0.05 so therefore we reject the hypothesis that the data come from 
# a normal (Gaussian) distribution


# Fit distributions
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

# The Beta distribution has the highest log-likelihood among the fitted
# models, therefore it provides the best fit


# Kolmogorov-Smirnov test
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

# Only the Beta distribution has p-value > 0.05
# Therefore it is the only hypothesis we cannot reject


# Plot histogram with fitted theoretical Beta probability density
g + hist +
  stat_function(
    fun = dbeta,
    args = as.list(fit.beta$estimate),
    colour="red",
    size=1.5
  )
