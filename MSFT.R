# Load necessary libraries, install if not present
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
library(dplyr)
library(ggplot2)
library(moments)
# Set the working directory and read the CSV files

MSFT <- read.csv("MSFT.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
return_MSFT <- as.numeric(gsub(",", ".", gsub("%", "", MSFT$Var.)))
return_sp500 <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Function to calculate excess returns
calc_excess_return <- function(stock_return, tbill_price) {
  stock_return - tbill_price
}

# Calculate excess returns for MSFT and S&P 500
excess_returns_MSFT <- calc_excess_return(return_MSFT, tbill$Price)
excess_returns_sp500 <- calc_excess_return(return_sp500, tbill$Price)

desc_stats <- function(data) {
  data <- na.omit(data)
  cat("\nAverage: ", mean(data), "\n")
  cat("Median: ", median(data), "\n")
  cat("Standard Deviation: ", sd(data), "\n")
  cat("Minimum: ", min(data), "\n")
  cat("Maximum: ", max(data), "\n")
  cat("Skewness: ", skewness(data), "\n")
  cat("Kurtosis: ", kurtosis(data), "\n")
}

# Print excess returns and descriptive statistics for MSFT and S&P 500
print(head(excess_returns_MSFT, 10))
cat("\nDescriptive Statistics for MSFT:")
desc_stats(excess_returns_MSFT)

print(head(excess_returns_sp500, 10))
cat("\nDescriptive Statistics for S&P 500:")
desc_stats(excess_returns_sp500)

# Function to calculate alpha and beta
calc_alpha_beta <- function(stock_excess_return, market_excess_return) {
  data <- na.omit(data.frame(Stock_Excess_Return = stock_excess_return, Market_Excess_Return = market_excess_return))
  lm(Stock_Excess_Return ~ Market_Excess_Return, data = data)
}

# Calculate and print alpha and beta for MSFT compared to S&P 500
model_MSFT_sp500 <- calc_alpha_beta(excess_returns_MSFT, excess_returns_sp500)
cat("\nModel Summary (MSFT vs S&P 500):\n")
print(summary(model_MSFT_sp500))

# Combine data into one dataframe for ggplot
combined_data <- na.omit(data.frame(
  Market_Excess_Return = excess_returns_sp500, 
  MSFT_Excess_Return = excess_returns_MSFT
))

# Create an aesthetically appealing scatter plot with a regression line
ggplot(combined_data, aes(x = Market_Excess_Return, y = MSFT_Excess_Return)) +
  geom_point(color = "darkorange", size = 1) +
  geom_smooth(method = "lm", color = "dodgerblue", size = 1) +
  labs(title = "Relationship between Excess Returns of MSFT and S&P 500",
       x = "Market (S&P 500) Excess Return (%)",
       y = "MSFT Excess Return (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
