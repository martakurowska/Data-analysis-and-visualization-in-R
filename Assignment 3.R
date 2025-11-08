# Global parameters
n <- 1e3        # no. of data points
m <- 2          # mean
s <- 0.5        # standard deviation

# Generate normal distribution data 
data <- rnorm(n, m, s)

# Create data histogram, no plot
h <- hist(data, plot = FALSE)  

# Theoretical probability density function, 100 samples
x_seq <- seq(min(h$breaks), max(h$breaks), length.out = 100)
thprobdens <- dnorm(x_seq, m, s)

# Determine y-axis limits
ylim_max <- max(c(h$density, thprobdens))

# Save figure to .png file
png ("a3_figure.png")

# Scatter plot of empirical probability density function
plot(h$mids, 
     h$density, 
     pch=19,
     xlab = "x",   # Axis labels
     ylab = "f(x)",
     xlim = range(h$breaks),   # Axis limits
     ylim = c(0, ylim_max)
     )

# Theoretical probability density function curve
# curve(dnorm(x, m, s), from = min(h$breaks), to = max(h$breaks), col = "red", lwd = 2, add = TRUE)
lines(x_seq, thprobdens, col = "red", lwd = 2)

# Add legend
legend ("topleft",
        legend = c("data", "fit"),
        lty = c (0, 1),
        pch = c (19, NA),
        col = c ("black","red")
        )

# Write plot to file
dev.off()

# Writes plot fo file but still generates plot preview 
# dev.copy(png, "a3_figure.png")
# dev.off()