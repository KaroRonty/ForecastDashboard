library(DT)
library(shiny)
library(dplyr)
library(fable)
library(plotly)
library(ggplot2)
library(forecast)
library(lubridate)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Slider which affects forecast horizon
      sliderInput("h", "Forecast horizon (months)", min = 1, max = 120, value = 24),
      checkboxInput("test_set_accuracies",
                    "Show only test set accuracies in the accuracy metrics")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs",
                 # 2x2 grid of graphs
                 fluidRow(
                   column(6,
                          plotlyOutput("p_arima")),
                   column(6,
                          plotlyOutput("p_ets")), #
                   column(6,
                          plotlyOutput("p_tbats")), #
                   column(6,
                          plotlyOutput("p_comb")))
                 ),
        tabPanel("Accuracy metrics",
                 DTOutput("metrics"))
      )
    )
  )
)
