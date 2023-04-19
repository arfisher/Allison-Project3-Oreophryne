# Load data from CSV file
data <- read.csv("your_data_file.csv", header = TRUE)

# Split data into two data frames, one for each genus
genus1 <- data[data$Genus == "Genus1", ]
genus2 <- data[data$Genus == "Genus2", ]

# Check the sample sizes for each genus
n1 <- nrow(genus1)
n2 <- nrow(genus2)
cat("Sample sizes: ", n1, "and", n2, "\n")

# Compute the means for each genus
mean1 <- mean(genus1$Measurement)
mean2 <- mean(genus2$Measurement)

# Compute the standard deviations for each genus
sd1 <- sd(genus1$Measurement)
sd2 <- sd(genus2$Measurement)

# Compute the t-statistic and p-value
t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
df <- n1 + n2 - 2
p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

# Print results
cat("Mean for Genus1:", mean1, "\n")
cat("Mean for Genus2:", mean2, "\n")
cat("Standard deviation for Genus1:", sd1, "\n")
cat("Standard deviation for Genus2:", sd2, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_val, "\n")

# Test for significance
if (p_val < 0.05) {
  cat("The difference between the two genera is statistically significant.\n")
} else {
  cat("The difference between the two genera is not statistically significant.\n")
}
