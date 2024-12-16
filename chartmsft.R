# Load necessary libraries, install if not present
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

# Read the CSV files
jpm <- read.csv("JPM.csv")
MSFT <- read.csv("MSFT.csv")
MSFT <- read.csv("MSFT.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
MSFT$Var. <- as.numeric(gsub(",", ".", gsub("%", "", MSFT$Var.)))
sp500$Change <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Convert the Date columns to Date type, ensuring consistent format
sp500$Date <- as.Date(sp500$Date, format = "%m/%d/%Y") # Assuming the sp500 dates are month/day/year
tbill$Date <- as.Date(tbill$Date, format = "%m/%d/%Y") # Assuming the tbill dates are month/day/year
MSFT$Data <- as.Date(MSFT$Data, format = "%d.%m.%Y")
# Rename and prepare for joining
tbill <- tbill %>% rename(Data = Date, RiskFreeRate = Price)

# Left join with tbill and calculate excess returns
MSFT <- MSFT %>% 
  rename(Return = Var.) %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Return - RiskFreeRate)

sp500 <- sp500 %>% 
  rename(Data = Date) %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Change - RiskFreeRate)

# Filter data for the specified date range
MSFT2008 <- MSFT %>% 
  filter(Data >= as.Date("2008-06-15"))

sp5002008 <- sp500 %>% 
  filter(Data >= as.Date("2008-06-15"))

# Filter data for the first date range: from 2008-06-16 to 2019-12-12
MSFT_2008_to_2019 <- MSFT %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

sp500_2008_to_2019 <- sp500 %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

# Filter data for the second date range: from 2019-12-13 to 2023-09-01
MSFT_2019_to_2023 <- MSFT %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))

sp500_2019_to_2023 <- sp500 %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))



# Prepare the data for ggplot
MSFT$data <- "Excess Return MSFT"
sp500$data <- "Excess Return S&P 500"

# Select only the necessary columns and rename for consistency
MSFT <- MSFT %>% select(Data, ExcessReturn, data)
sp500 <- sp500 %>% select(Data, ExcessReturn, data)


# Combine the excess return datasets into one for ggplot
excess_data <- rbind(MSFT, sp500)

# Sort and calculate cumulative return
excess_data <- excess_data %>%
  arrange(data, Data) %>%
  group_by(data) %>%
  mutate(CumulativeExcessReturn = cumsum(ExcessReturn))

# Calculate beta for MSFT against S&P 500 using excess returns
MSFT_with_sp500 <- MSFT %>% 
  select(Data, ExcessReturn) %>%
  rename(MSFT_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500 %>% select(Data, ExcessReturn) %>% rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit <- lm(MSFT_ExcessReturn ~ SP500_ExcessReturn, data = MSFT_with_sp500)
# Calculate the beta value
beta_value <- round(beta_fit$coefficients["SP500_ExcessReturn"], 2)

# Calculate beta for MSFT against S&P 500 using excess returns for the new period
MSFT_with_sp500_2008 <- MSFT2008 %>%
  select(Data, ExcessReturn) %>%
  rename(MSFT_ExcessReturn = ExcessReturn) %>%
  inner_join(sp5002008 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008 <- lm(MSFT_ExcessReturn ~ SP500_ExcessReturn, data = MSFT_with_sp500_2008)
beta_2008 <- round(beta_fit_2008$coefficients["SP500_ExcessReturn"], 2)
alpha_2008 <- round(beta_fit_2008$coefficients["(Intercept)"], 2)


# Calculate beta for MSFT against S&P 500 using excess returns for the first date range (2008-06-16 to 2019-12-12)
MSFT_with_sp500_2008_to_2019 <- MSFT_2008_to_2019 %>%
  select(Data, ExcessReturn) %>%
  rename(MSFT_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2008_to_2019 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008_to_2019 <- lm(MSFT_ExcessReturn ~ SP500_ExcessReturn, data = MSFT_with_sp500_2008_to_2019)
beta_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["SP500_ExcessReturn"], 2)
alpha_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["(Intercept)"], 2)

# Calculate beta for MSFT against S&P 500 using excess returns for the second date range (2019-12-13 to 2023-09-01)
MSFT_with_sp500_2019_to_2023 <- MSFT_2019_to_2023 %>%
  select(Data, ExcessReturn) %>%
  rename(MSFT_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2019_to_2023 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2019_to_2023 <- lm(MSFT_ExcessReturn ~ SP500_ExcessReturn, data = MSFT_with_sp500_2019_to_2023)
beta_2019_to_2023 <- round(beta_fit_2019_to_2023$coefficients["SP500_ExcessReturn"], 2)
alpha_2019_to_2023 <- round(beta_fit_2019_to_2023$coefficients["(Intercept)"], 2)


final_plot <- ggplot(excess_data, aes(x = Data, y = CumulativeExcessReturn, color = data)) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12), # Aumenta la grandezza del testo della leggenda
    legend.title = element_text(size = 13), # Aumenta la grandezza del titolo della leggenda se necessario
    legend.key.size = unit(2, 'lines'), # Aumenta la grandezza dei simboli nella leggenda
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Cumulative Excess Return of MSFT and S&P 500",
    subtitle = paste("Beta of MSFT (against S&P 500 Excess Returns):", beta_value),
    x = "Date",
    y = "Cumulative Excess Return (%)",
    color = ""
  )

# Add a text annotation for the beta value
final_plot + annotate("text", x = max(excess_data$Data), y = min(excess_data$CumulativeExcessReturn), 
                      label = paste("Beta:", beta_value), 
                      hjust = 1, vjust = 0, size = 5, color = "black")

# Note: Adjust `hjust` and `vjust` to position the annotation inside the plot area appropriately.

financial_crisis <- as.Date("2008-06-15")
covid_crash <- as.Date("2019-12-12")

# Add vertical lines for significant events
final_plot <- final_plot +
  geom_vline(xintercept = as.numeric(financial_crisis), color = "red", linetype = "longdash") +
  geom_vline(xintercept = as.numeric(covid_crash), color = "red", linetype = "longdash") +
  annotate("text", x = financial_crisis, y = Inf, label = "2008 Crisis", angle = 95, vjust = 1.1, hjust = 2.1) +
  annotate("text", x = covid_crash, y = Inf, label = "COVID", angle = 95, vjust = 1.1, hjust = 4) 

final_plot <- final_plot +
  annotate("text", x = max(excess_data$Data), y = min(excess_data$CumulativeExcessReturn), 
           label = paste("Beta:", beta_2008), 
           hjust = 10.5, vjust = -20, size = 4.5, color = "black")+
  annotate("text", x = max(excess_data$Data), y = min(excess_data$CumulativeExcessReturn), 
           label = paste("Beta (2008-2019):", beta_2008_to_2019), 
           hjust = 2.8, vjust = -20, size = 4.5, color = "black") +
  annotate("text", x = max(excess_data$Data), y = min(excess_data$CumulativeExcessReturn) - 2, 
           label = paste("Beta (2019-2023):", beta_2019_to_2023), 
           hjust = 0.9, vjust = -20, size = 4.5, color = "black")

# Print the final enhanced plot with vertical lines
print(final_plot)
