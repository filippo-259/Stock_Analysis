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
googl <- read.csv("GOOGL.csv")
amzn <- read.csv("AMZN.csv")
MSFT <- read.csv("MSFT.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
jpm$Var. <- as.numeric(gsub(",", ".", gsub("%", "", jpm$Var.)))
googl$Var. <- as.numeric(gsub(",", ".", gsub("%", "", googl$Var.)))
amzn$Change <- as.numeric(gsub(",", ".", gsub("%", "", amzn$Change)))
MSFT$Var. <- as.numeric(gsub(",", ".", gsub("%", "", MSFT$Var.)))
sp500$Change <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Assuming the Date column in tbill and in the correct format
tbill <- tbill %>% rename(Data = Date, RiskFreeRate = Price)
tbill$Data <- as.Date(tbill$Data, format = "%m/%d/%Y")

# Make sure all Data columns are Date type before merging
amzn <- amzn %>% mutate(Data = as.Date(Date, format = "%m/%d/%Y"))
sp500 <- sp500 %>% mutate(Data = as.Date(Date, format = "%m/%d/%Y"))

# Left join with tbill and calculate excess returns
amzn <- amzn %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Change - RiskFreeRate)

sp500 <- sp500 %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Change - RiskFreeRate)

# Filter data for the specified date range
amzn2008 <- amzn %>% 
  filter(Data >= as.Date("2008-06-15"))

sp5002008 <- sp500 %>% 
  filter(Data >= as.Date("2008-06-15"))

# Filter data for the first date range: from 2008-06-16 to 2019-12-12
amzn_2008_to_2019 <- amzn %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

sp500_2008_to_2019 <- sp500 %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

# Filter data for the second date range: from 2019-12-13 to 2023-09-01
amzn_2019_to_2023 <- amzn %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))

sp500_2019_to_2023 <- sp500 %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))



# Prepare the data for ggplot
amzn$data <- "Excess Return AMZN"
sp500$data <- "Excess Return S&P 500"

# Select only the necessary columns and rename for consistency
amzn <- amzn %>% select(Data, ExcessReturn, data)
sp500 <- sp500 %>% select(Data, ExcessReturn, data)


# Combine the excess return datasets into one for ggplot
excess_data <- rbind(amzn, sp500)

# Sort and calculate cumulative return
excess_data <- excess_data %>%
  arrange(data, Data) %>%
  group_by(data) %>%
  mutate(CumulativeExcessReturn = cumsum(ExcessReturn))

# Calculate beta for AMZN against S&P 500 using excess returns
amzn_with_sp500 <- amzn %>% 
  select(Data, ExcessReturn) %>%
  rename(AMZN_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500 %>% select(Data, ExcessReturn) %>% rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit <- lm(AMZN_ExcessReturn ~ SP500_ExcessReturn, data = amzn_with_sp500)
# Calculate the beta value
beta_value <- round(beta_fit$coefficients["SP500_ExcessReturn"], 2)

# Calculate beta for AMZN against S&P 500 using excess returns for the new period
amzn_with_sp500_2008 <- amzn2008 %>%
  select(Data, ExcessReturn) %>%
  rename(AMZN_ExcessReturn = ExcessReturn) %>%
  inner_join(sp5002008 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008 <- lm(AMZN_ExcessReturn ~ SP500_ExcessReturn, data = amzn_with_sp500_2008)
beta_2008 <- round(beta_fit_2008$coefficients["SP500_ExcessReturn"], 2)
alpha_2008 <- round(beta_fit_2008$coefficients["(Intercept)"], 2)


# Calculate beta for AMZN against S&P 500 using excess returns for the first date range (2008-06-16 to 2019-12-12)
amzn_with_sp500_2008_to_2019 <- amzn_2008_to_2019 %>%
  select(Data, ExcessReturn) %>%
  rename(AMZN_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2008_to_2019 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008_to_2019 <- lm(AMZN_ExcessReturn ~ SP500_ExcessReturn, data = amzn_with_sp500_2008_to_2019)
beta_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["SP500_ExcessReturn"], 2)
alpha_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["(Intercept)"], 2)

# Calculate beta for AMZN against S&P 500 using excess returns for the second date range (2019-12-13 to 2023-09-01)
amzn_with_sp500_2019_to_2023 <- amzn_2019_to_2023 %>%
  select(Data, ExcessReturn) %>%
  rename(AMZN_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2019_to_2023 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2019_to_2023 <- lm(AMZN_ExcessReturn ~ SP500_ExcessReturn, data = amzn_with_sp500_2019_to_2023)
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
    title = "Cumulative Excess Return of AMZN and S&P 500",
    subtitle = paste("Beta of AMZN (against S&P 500 Excess Returns):", beta_value),
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
