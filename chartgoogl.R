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
googl <- read.csv("GOOGL.csv")
sp500 <- read.csv("sp500.csv")
tbill <- read.csv("tbill-1m.csv")

# Convert percentage strings to numeric values
googl$Var. <- as.numeric(gsub(",", ".", gsub("%", "", googl$Var.)))
sp500$Change <- as.numeric(gsub(",", ".", gsub("%", "", sp500$Change)))

# Convert the Date columns to Date type, ensuring consistent format
sp500$Date <- as.Date(sp500$Date, format = "%m/%d/%Y") # Assuming the sp500 dates are month/day/year
tbill$Date <- as.Date(tbill$Date, format = "%m/%d/%Y") # Assuming the tbill dates are month/day/year
googl$Data <- as.Date(googl$Data, format = "%d.%m.%Y")
# Rename and prepare for joining
tbill <- tbill %>% rename(Data = Date, RiskFreeRate = Price)

# Left join with tbill and calculate excess returns
googl <- googl %>% 
  rename(Return = Var.) %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Return/100 - RiskFreeRate/100)

sp500 <- sp500 %>% 
  rename(Data = Date) %>% 
  left_join(tbill, by = "Data") %>% 
  mutate(ExcessReturn = Change/100 - RiskFreeRate/100)

# Filter data for the specified date range
googl2008 <- googl %>% 
  filter(Data >= as.Date("2008-06-15"))

sp5002008 <- sp500 %>% 
  filter(Data >= as.Date("2008-06-15"))

# Filter data for the first date range: from 2008-06-16 to 2019-12-12
googl_2008_to_2019 <- googl %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

sp500_2008_to_2019 <- sp500 %>%
  filter(Data >= as.Date("2008-06-16") & Data <= as.Date("2019-12-12"))

# Filter data for the second date range: from 2019-12-13 to 2023-09-01
googl_2019_to_2023 <- googl %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))

sp500_2019_to_2023 <- sp500 %>%
  filter(Data >= as.Date("2019-12-13") & Data <= as.Date("2023-09-01"))



# Prepare the data for ggplot
googl$data <- "Excess Return googl"
sp500$data <- "Excess Return S&P 500"

# Select only the necessary columns and rename for consistency
googl <- googl %>% select(Data, ExcessReturn, data)
sp500 <- sp500 %>% select(Data, ExcessReturn, data)


# Combine the excess return datasets into one for ggplot
excess_data <- rbind(googl, sp500)

# Sort and calculate cumulative return
excess_data <- excess_data %>%
  arrange(data, Data) %>%
  group_by(data) %>%
  mutate(CumulativeExcessReturn = cumsum(ExcessReturn))

# Calculate beta for googl against S&P 500 using excess returns
googl_with_sp500 <- googl %>% 
  select(Data, ExcessReturn) %>%
  rename(googl_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500 %>% select(Data, ExcessReturn) %>% rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit <- lm(googl_ExcessReturn ~ SP500_ExcessReturn, data = googl_with_sp500)
# Calculate the beta value
beta_value <- round(beta_fit$coefficients["SP500_ExcessReturn"], 2)
alpha_value <- round(beta_fit$coefficients["(Intercept)"], 2)

# Calculate beta for googl against S&P 500 using excess returns for the new period
googl_with_sp500_2008 <- googl2008 %>%
  select(Data, ExcessReturn) %>%
  rename(googl_ExcessReturn = ExcessReturn) %>%
  inner_join(sp5002008 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008 <- lm(googl_ExcessReturn ~ SP500_ExcessReturn, data = googl_with_sp500_2008)
beta_2008 <- round(beta_fit_2008$coefficients["SP500_ExcessReturn"], 2)
alpha_2008 <- round(beta_fit_2008$coefficients["(Intercept)"], 2)


# Calculate beta for googl against S&P 500 using excess returns for the first date range (2008-06-16 to 2019-12-12)
googl_with_sp500_2008_to_2019 <- googl_2008_to_2019 %>%
  select(Data, ExcessReturn) %>%
  rename(googl_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2008_to_2019 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2008_to_2019 <- lm(googl_ExcessReturn ~ SP500_ExcessReturn, data = googl_with_sp500_2008_to_2019)
beta_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["SP500_ExcessReturn"], 2)
alpha_2008_to_2019 <- round(beta_fit_2008_to_2019$coefficients["(Intercept)"], 2)

# Calculate beta for googl against S&P 500 using excess returns for the second date range (2019-12-13 to 2023-09-01)
googl_with_sp500_2019_to_2023 <- googl_2019_to_2023 %>%
  select(Data, ExcessReturn) %>%
  rename(googl_ExcessReturn = ExcessReturn) %>%
  inner_join(sp500_2019_to_2023 %>%
               select(Data, ExcessReturn) %>%
               rename(SP500_ExcessReturn = ExcessReturn), by = "Data")

beta_fit_2019_to_2023 <- lm(googl_ExcessReturn ~ SP500_ExcessReturn, data = googl_with_sp500_2019_to_2023)
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
    title = "Cumulative Excess Return of googl and S&P 500",
    subtitle = paste("β / α of GOOGL (against S&P 500 Excess Returns):", beta_value,"/", alpha_value),
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

# Define the positions for the annotations to avoid overlap
last_date <- max(excess_data$Data)
min_return <- min(excess_data$CumulativeExcessReturn)
max_return <- max(excess_data$CumulativeExcessReturn)

# Assumiamo che le date centrali per ogni periodo siano già state calcolate o stimate
central_date_pre_2008 <- as.Date("2004-06-15") # Data approssimativa a metà del periodo pre-2008
central_date_2008_2019 <- as.Date("2013-12-31") # Data approssimativa a metà del periodo 2008-2019
central_date_post_2019 <- as.Date("2021-06-15") # Data approssimativa a metà del periodo post-2019

# Trova una posizione y che sia circa al centro del range di y nel grafico
mid_y <- (max(excess_data$CumulativeExcessReturn) + min(excess_data$CumulativeExcessReturn)) / 2

# Aggiungi le etichette al grafico, posizionandole in mezzo verticalmente
final_plot <- final_plot +
  annotate("text", x = as.Date("2006-01-01"), y = mid_y, 
           label = paste("β (2004-2008):", beta_2008, "\nα (2004-2008):", alpha_2008), 
           hjust = 0.5, vjust = 2, size = 4.5, color = "black") +
  annotate("text", x = as.Date("2014-01-01"), y = mid_y, 
           label = paste("β (2008-2019):", beta_2008_to_2019, "\nα (2008-2019):", alpha_2008_to_2019), 
           hjust = 0.5, vjust = 1, size = 4.5, color = "black") +
  annotate("text", x = as.Date("2021-01-01"), y = mid_y, 
           label = paste("β (2019-2023):", beta_2019_to_2023, "\nα (2019-2023):", alpha_2019_to_2023), 
           hjust = 0.5, vjust = 1, size = 4.5, color = "black")

# Ora stampa il grafico
print(final_plot)
