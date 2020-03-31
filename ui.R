library(shiny)
library(dplyr)
library(fable)
library(ggplot2)
library(tsibble)
library(forecast)
library(lubridate)

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