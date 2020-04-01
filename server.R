library(shiny)
library(dplyr)
library(fable)
library(plotly)
library(ggplot2)
library(tsibble)
library(forecast)
library(lubridate)

# Load the data and formate dates
visitors <- read.csv("website_visitors.csv") %>% 
  mutate(dates = ymd(dates)) %>% 
  rename(amount = visitors) %>% 
  as_tsibble(regular = FALSE)

# Make the models
m_arima <- auto.arima(visitors$amount)
m_ets <- ets(visitors$amount)
m_tbats <- tbats(visitors$amount)

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
    d_comb <- tibble(Day = c(1:nrow(visitors), nrow(visitors) + 1:h),
                     Data = c(visitors$amount, rep(NA, h)),
                     Forecast = c(rep(NA, nrow(visitors)), m_comb))
    
    # Plot without forecasting intervals
    p1 <- ggplot(d_comb, aes(x = Day)) +
      geom_line(aes(y = Data), color = "black") +
      geom_line(aes(y = Forecast), color = "#00BFC4") +
      ggtitle("Combination forecast") +
      theme_minimal()
    
  } else {
    # Create a tibble containing time, actuals, forecasts and intervals
    fcasts <- tibble(Day = c(1:nrow(visitors), nrow(visitors) + 1:h),
                     Data = c(visitors$amount, rep(NA, h)),
                     Forecast = c(rep(NA, nrow(visitors)), forecast$mean),
                     lwr80 = c(rep(NA, nrow(visitors)), forecast$lower[, 1]),
                     lwr95 = c(rep(NA, nrow(visitors)), forecast$lower[, 2]),
                     upr80 = c(rep(NA, nrow(visitors)), forecast$upper[, 1]),
                     upr95 = c(rep(NA, nrow(visitors)), forecast$upper[, 2]))
    
    p1 <- ggplot(fcasts, aes(x = Day, y = Data)) +
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
}
