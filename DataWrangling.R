library(dplyr)
library(lubridate)

data <- read.csv("visitors.csv")

df <- data %>% 
  group_by(Period) %>% 
  summarise(visitors = sum(Total)) %>% 
  mutate(dates = ymd(paste0(Period, "-01"))) %>% 
  select(dates, visitors)

# write.csv(df, "website_visitors.csv", row.names = FALSE)
