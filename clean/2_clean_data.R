library(openxlsx)
library(tidyverse)
library(lubridate)
library(zoo)
library(viridis)


# avg_data <- read.xlsx("C:/Carey/Projects/unemployment_project/data/averages_2019.xlsx")
# monthly_data <- read.xlsx("C:/Carey/Projects/unemployment_project/data/current_unemployment.xlsx")


# Cleaning data -----------------------------------------------------------

colnames(monthly_data) <- c("Series", "StateCode", "CountyCode", "CountyState", 
                            "Period", "Force", "Employed", "Unemployed", "Percent")
monthly_data <- monthly_data %>% 
  tail(nrow(monthly_data) - 5) %>% 
  mutate(Period = as.yearmon(Period, "%b-%y"),
         Unemployed = as.numeric(Unemployed),
         Percent = as.numeric(Percent),
         Force = as.numeric(Force),
         LogForce = log(Force, base = 10))

colnames(avg_data) <- c("Series", "StateCode", "CountyCode", "CountyState", 
                            "Period", "X", "Force", "Employed", "Unemployed", "Percent")
avg_data <- avg_data %>% 
  tail(nrow(avg_data) - 5) %>% 
  mutate(Period = as.yearmon(Period, "%y"),
         Unemployed = as.numeric(Unemployed),
         Employ19 = as.numeric(Percent),
         Force = as.numeric(Force),
         LogForce = log(Force, base = 10)) %>% 
  select(CountyState, Employ19)

getSize <- monthly_data %>% 
  group_by(CountyState) %>% 
  summarise(AvgSizeRound = floor(mean(LogForce)),
            AvgSize = mean(LogForce),
            PeakUnemp = max(Percent)) %>% 
  mutate(AvgSizeRound = as.factor(AvgSizeRound))

monthly_data <- monthly_data %>% 
  left_join(getSize, by = "CountyState") %>% 
  left_join(avg_data, by = "CountyState") %>% 
  separate(CountyState, c("County", "State"), sep = ",") %>% 
  mutate(State = trimws(State),
         Recovery = 100 * (PeakUnemp - Percent) / pmax(PeakUnemp - Employ19, 1)) %>% 
  filter(State != "PR")

write.csv(monthly_data, file = "C:/Carey/Projects/unemployment_project/data/cleaned_data.csv",
          row.names = FALSE)


monthly_data %>%
  filter(!is.na(AvgSizeRound)) %>%
  group_by(Period, AvgSizeRound) %>%
  summarise(AggPercent = sum(100 * Unemployed) / sum(Force),
            AvgPercent = sum(Force * Employ19) / sum(Force),
            Recovery = sum(Force * Recovery) / sum(Force)) %>%
  ggplot() +
  geom_line(aes(Period, Recovery, group = AvgSizeRound, col = as.factor(AvgSizeRound)), size = 1) +
  geom_point(aes(Period, Recovery, group = AvgSizeRound, col = as.factor(AvgSizeRound)), size = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = "Time",
       y = "Unemployment [%]",
       color = "Log_10 pop size",
       title = "blah") +
  scale_x_yearmon(format = "%h%y",
                  breaks = unique(monthly_data$Period)) +
  scale_y_continuous(breaks = c(seq(0, 200, by = 5)))
