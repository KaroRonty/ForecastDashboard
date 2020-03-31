library(data.table)
library(dtplyr)
library(dplyr)

options(scipen = 1e9)

data1 <- fread("train_1.csv")
data2 <- fread("train_2.csv")

data <- data1 %>%
  lazy_dt() %>% 
  full_join(data2) %>% 
  select(-Page) %>% 
  as_tibble()

data_sums <- apply(data, 2, function(x) sum(x, na.rm = TRUE))

df <- data.frame(dates = as.Date(names(data_sums)),
                 visitors = data_sums)

# write.csv(df, "website_visitors.csv", row.names = FALSE)
