monthly_data <- read.csv("C:/Carey/Projects/unemployment_project/data/cleaned_data.csv") %>% 
  mutate(Period = as.yearmon(Period, "%b %Y"))
