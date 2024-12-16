# Load necessary libraries, install if not present
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
library(dplyr)
library(ggplot2)
library(moments)
# Set the working directory and read the CSV files

googl <- read.csv("GOOGL.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
return_googl <- as.numeric(gsub(",", ".", gsub("%", "", googl$Var.)))
return_sp500 <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Function to calculate excess returns
calc_excess_return <- function(stock_return, tbill_price) {
  stock_return - tbill_price
}

# Calculate excess returns for google and S&P 500
excess_returns_googl <- calc_excess_return(return_googl, tbill$Price)
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

# Print excess returns and descriptive statistics for google and S&P 500
print(head(excess_returns_googl, 10))
cat("\nDescriptive Statistics for google:")
desc_stats(excess_returns_googl)

print(head(excess_returns_sp500, 10))
cat("\nDescriptive Statistics for S&P 500:")
desc_stats(excess_returns_sp500)

# Function to calculate alpha and beta
calc_alpha_beta <- function(stock_excess_return, market_excess_return) {
  data <- na.omit(data.frame(Stock_Excess_Return = stock_excess_return, Market_Excess_Return = market_excess_return))
  lm(Stock_Excess_Return ~ Market_Excess_Return, data = data)
}

# Calculate and print alpha and beta for google compared to S&P 500
model_googl_sp500 <- calc_alpha_beta(excess_returns_googl, excess_returns_sp500)
cat("\nModel Summary (google vs S&P 500):\n")
print(summary(model_googl_sp500))

# Combine data into one dataframe for ggplot
combined_data <- na.omit(data.frame(
  Market_Excess_Return = excess_returns_sp500, 
  googl_Excess_Return = excess_returns_googl
))

# Create an aesthetically appealing scatter plot with a regression line
ggplot(combined_data, aes(x = Market_Excess_Return, y = googl_Excess_Return)) +
  geom_point(color = "darkorange", size = 1) +
  geom_smooth(method = "lm", color = "dodgerblue", size = 1) +
  labs(title = "Relationship between Excess Returns of google and S&P 500",
       x = "Market (S&P 500) Excess Return (%)",
       y = "google Excess Return (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Calcola le statistiche descrittive per Google e S&P 500
stats_googl <- c(mean = mean(excess_returns_googl, na.rm = TRUE),
                 median = median(excess_returns_googl, na.rm = TRUE),
                 sd = sd(excess_returns_googl, na.rm = TRUE),
                 min = min(excess_returns_googl, na.rm = TRUE),
                 max = max(excess_returns_googl, na.rm = TRUE),
                 skewness = skewness(excess_returns_googl, na.rm = TRUE),
                 kurtosis = kurtosis(excess_returns_googl, na.rm = TRUE))

stats_sp500 <- c(mean = mean(excess_returns_sp500, na.rm = TRUE),
                 median = median(excess_returns_sp500, na.rm = TRUE),
                 sd = sd(excess_returns_sp500, na.rm = TRUE),
                 min = min(excess_returns_sp500, na.rm = TRUE),
                 max = max(excess_returns_sp500, na.rm = TRUE),
                 skewness = skewness(excess_returns_sp500, na.rm = TRUE),
                 kurtosis = kurtosis(excess_returns_sp500, na.rm = TRUE))

# Crea un dataframe per il grafico a barre
stats_df <- data.frame(
  Statistic = c("Mean", "Median", "SD", "Min", "Max", "Skewness", "Kurtosis"),
  Google = stats_googl,
  SP500 = stats_sp500
)

# Grafico a barre
ggplot(stats_df, aes(x = Statistic)) +
  geom_bar(aes(y = Google, fill = "Google"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = SP500, fill = "S&P 500"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Google" = "grey", "S&P 500" = "orange")) +
  labs(title = "Comparative Descriptive Statistics: Google vs S&P 500",
       x = "Statistic", y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
