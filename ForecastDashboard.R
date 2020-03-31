library(shiny)
library(dplyr)
library(fable)
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Slider which affects forecast horizon
      sliderInput("h", "Forecast horizon", min = 1, max = 120, value = 60)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs",
                 # 2x2 grid of graphs
                 fluidRow(
                   column(6,
                          plotOutput("p_arima")),
                   column(6,
                          plotOutput("p_ets")), #
                   column(6,
                          plotOutput("p_tbats")), #
                   column(6,
                          plotOutput("p_comb")))
        )
      )
    )
  )
)

server <- function(input, output, session){
  # ARIMA plot
  output$p_arima <- renderPlot({
    # Make forecasts by getting h from slider
    f_arima <<- m_arima %>% forecast(h = input$h) 
    
    f_arima %>% autoplot() +
      autolayer(f_arima, color = "#F8766D") +
      ylim(c(0.7e8, 3.5e8))
  })
  # ETS plot
  output$p_ets <- renderPlot({
    # Make forecasts by getting h from slider
    f_ets <<- m_ets %>% forecast(h = input$h)
    
    f_ets %>% autoplot() + 
      autolayer(f_ets, color = "#C77CFF") +
      ylim(c(0.7e8, 3.5e8))
  })
  # TBATS plot
  output$p_tbats <- renderPlot({
    # Make forecasts by getting h from slider
    f_tbats <<- m_tbats %>% forecast(h = input$h)
    
    f_tbats %>% autoplot() + 
      autolayer(f_tbats, color = "#7CAE00") +
      ylim(c(0.7e8, 3.5e8))
  })
  # Combination plot
  output$p_comb <- renderPlot({
    m_comb <- (f_arima$mean + f_ets$mean + f_tbats$mean) / 3
    
    # Make a tibble containing time, actuals and predictions for plotting
    d_comb <- tibble(Time = c(1:nrow(visitors), nrow(visitors) + 1:input$h),
                     Actual = c(visitors$amount, rep(NA, input$h)),
                     Prediction = c(rep(NA, nrow(visitors)), m_comb))
    
    # Plot without forecasting intervals
    ggplot(d_comb, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "A")) +
      geom_line(aes(y = Prediction, color = "P")) +
      ggtitle("Combination forecast") +
      ylim(c(0.7e8, 3.5e8)) +
      scale_colour_manual("",
                          breaks = c("A", "P"),
                          values = c("black", "#00BFC4"))
  })
  
}

shinyApp(ui, server)
