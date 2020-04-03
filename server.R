library(DT)
library(shiny)
library(dplyr)
library(fable)
library(plotly)
library(ggplot2)
library(forecast)
library(lubridate)

# Load the data and formate dates
df <- read.csv("website_visitors.csv") %>% 
  mutate(dates = ymd(dates)) %>% 
  rename(amount = visitors)

visitors <- ts(df$amount,
               start = c(year(min(df$dates)), month(min(df$dates))),
               frequency = 12)

n_obs <- length(visitors)

# Make the models
m_arima <- auto.arima(visitors)
m_ets <- ets(visitors)
m_tbats <- tbats(visitors)

# Splint into training and test sets using 70/30 split
split_date <- df$dates[0.7 * nrow(df)]

visitors_training <- window(visitors, end = c(year(split_date),
                                              month(split_date)))

visitors_test <- window(visitors, start = c(year(split_date + months(1)),
                                            month(split_date + months(1))))

# Make models using the training data
c_arima <- auto.arima(visitors_training) %>%
  forecast(h = length(visitors_test))

c_ets <- ets(visitors_training) %>%
  forecast(h = length(visitors_test))

c_tbats <- tbats(visitors_training) %>%
  forecast(h = length(visitors_test))

c_combination <- (c_arima$mean + c_ets$mean + c_tbats$mean) / 3

# Compute accuracies for both sets using the models
a_arima <- c_arima %>% 
  accuracy(visitors_test)

a_ets <- c_ets %>% 
  accuracy(visitors_test)

a_tbats <- c_tbats %>% 
  accuracy(visitors_test)

a_combination <- c_combination %>%
  accuracy(visitors_test)

# Put the accuracy metrics into tibble
accuracies <- a_arima %>% 
  as_tibble() %>% 
  mutate(Model = "ARIMA",
         Set = c("Training", "Test")) %>% 
  rbind(a_combination %>% 
          as_tibble() %>% 
          mutate(Model = "Combination",
                 Set = c("Test"),
                 MASE = NA)) %>% 
  rbind(a_ets %>% 
          as_tibble() %>% 
          mutate(Model = "ETS",
                 Set = c("Training", "Test"))) %>% 
  rbind(a_tbats %>% 
          as_tibble() %>% 
          mutate(Model = "TBATS",
                 Set = c("Training", "Test"))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 3), nsmall = 3)) %>% 
  select(Model, Set, everything(), -ME, -MAE, -MPE, -ACF1) %>% 
  arrange(Set, Model)

# Function for plotting interactive plots
make_plots <- function(forecast = NULL,
                       model = NULL,
                       combination = FALSE,
                       h,
                       color = NULL,
                       eighty = NULL,
                       ninetyfive = NULL){
  if(combination){
    m_comb <- (f_arima$mean + f_ets$mean + f_tbats$mean) / 3
    
    # Make a tibble containing time, actuals and forecasts for plotting
    d_comb <- tibble(Date = c(df$dates,
                             seq.Date(last(df$dates) + months(1),
                                      to = last(df$dates) + months(h),
                                      by = "months")),
                     Data = c(as.numeric(visitors), rep(NA, h)),
                     Forecast = c(rep(NA, n_obs), m_comb))
    
    # Plot without forecasting intervals
    p1 <- ggplot(d_comb, aes(x = Date)) +
      geom_line(aes(y = Data), color = "black") +
      geom_line(aes(y = Forecast), color = "#00BFC4") +
      ggtitle("Combination forecast") +
      theme_minimal()
    
  } else {
    # Create a tibble containing time, actuals, forecasts and intervals
    fcasts <- tibble(Date = c(df$dates,
                             seq.Date(last(df$dates) + months(1),
                                      to = last(df$dates) + months(h),
                                      by = "months")),
                     Data = c(visitors, rep(NA, h)),
                     Forecast = c(rep(NA, n_obs), forecast$mean),
                     lwr80 = c(rep(NA, n_obs), forecast$lower[, 1]),
                     lwr95 = c(rep(NA, n_obs), forecast$lower[, 2]),
                     upr80 = c(rep(NA, n_obs), forecast$upper[, 1]),
                     upr95 = c(rep(NA, n_obs), forecast$upper[, 2]))
    
    p1 <- ggplot(fcasts, aes(x = Date, y = Data)) +
      geom_line() +
        geom_ribbon(aes(ymin = lwr80, ymax = upr80), fill = eighty) +
        geom_ribbon(aes(ymin = lwr95, ymax = upr95), fill = ninetyfive,
                    alpha = 0.7) +
      geom_line(aes(y = Forecast), color = color) +
      ggtitle(paste(as.character(model))) +
      theme_minimal()
    }
  
  return(ggplotly(p1))
}

server <- function(input, output, session){
  
  # ARIMA plot
  output$p_arima <- renderPlotly({
    # Make forecasts by getting h from slider
    f_arima <<- m_arima %>% forecast(h = input$h) 
    
    make_plots(f_arima, m_arima, h = input$h,
               color = "red", eighty = "#F8766D", ninetyfive = "#FAACA7")
  })
  # ETS plot
  output$p_ets <- renderPlotly({
    # Make forecasts by getting h from slider
    f_ets <<- m_ets %>% forecast(h = input$h)
    
    make_plots(f_ets, m_ets, h = input$h,
               color = "purple", eighty = "#C67CFF", ninetyfive = "#DCB0FF")
  })
  # TBATS plot
  output$p_tbats <- renderPlotly({
    # Make forecasts by getting h from slider
    f_tbats <<- m_tbats %>% forecast(h = input$h)
    
    make_plots(f_tbats, m_tbats, h = input$h,
               color = "olivedrab", eighty = "#7DAE00", ninetyfive = "#B1CE66")
  })
  # Combination plot
  observeEvent(input$h, {output$p_comb <- renderPlotly({
    make_plots(combination = TRUE, h = input$h)
  })
  })
  
  compute_accuracies <- reactive({
    if(input$test_set_accuracies == FALSE){
      return(accuracies)
    } else {
      return(accuracies %>% filter(Set == "Test"))
    }
  })
  
  # Accuracy metrics table
  output$metrics <- renderDT({
    compute_accuracies()
  })
  # Test set accuracies
  observeEvent(input$test_set_accuracies, {
    compute_accuracies()
  })
}
