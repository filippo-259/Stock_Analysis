if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
library(dplyr)
library(ggplot2)
library(moments)
# Set the working directory and read the CSV files

amzn <- read.csv("AMZN.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
return_amzn <- as.numeric(gsub(",", ".", gsub("%", "", amzn$Change)))
return_sp500 <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Function to calculate excess returns
calc_excess_return <- function(stock_return, tbill_price) {
  stock_return - tbill_price
}

# Calculate excess returns for Amazon and S&P 500
excess_returns_amzn <- calc_excess_return(return_amzn, tbill$Price)
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

# Print excess returns and descriptive statistics for Amazon and S&P 500
print(head(excess_returns_amzn, 10))
cat("\nDescriptive Statistics for Amazon:")
desc_stats(excess_returns_amzn)

print(head(excess_returns_sp500, 10))
cat("\nDescriptive Statistics for S&P 500:")
desc_stats(excess_returns_sp500)

# Function to calculate alpha and beta
calc_alpha_beta <- function(stock_excess_return, market_excess_return) {
  data <- na.omit(data.frame(Stock_Excess_Return = stock_excess_return, Market_Excess_Return = market_excess_return))
  lm(Stock_Excess_Return ~ Market_Excess_Return, data = data)
}

# Calculate and print alpha and beta for Amazon compared to S&P 500
model_amzn_sp500 <- calc_alpha_beta(excess_returns_amzn, excess_returns_sp500)
cat("\nModel Summary (Amazon vs S&P 500):\n")
print(summary(model_amzn_sp500))

# Combine data into one dataframe for ggplot
combined_data <- na.omit(data.frame(
  Market_Excess_Return = excess_returns_sp500, 
  Amazon_Excess_Return = excess_returns_amzn
))

# Create an aesthetically appealing scatter plot with a regression line
ggplot(combined_data, aes(x = Market_Excess_Return, y = Amazon_Excess_Return)) +
  geom_point(color = "darkorange", size = 1) +
  geom_smooth(method = "lm", color = "dodgerblue", size = 1) +
  labs(title = "Relationship between Excess Returns of Amazon and S&P 500",
       x = "Market (S&P 500) Excess Return (%)",
       y = "Amazon Excess Return (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
