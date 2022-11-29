library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(pool)
library(dygraphs)
library(DT)

fluidPage(
  fluidRow(
    column(3,
           radioButtons("tickers_rb", "Choose ticker:", "")),
    column(3,
           radioButtons("dates_rb", "Choose date:", "")),
    column(6,
           tags$b("Choose order-book plot (ordered by share in trading day volume):"),
           dataTableOutput("obplots_rtbl"))
  ),
  plotOutput("tdplot"),
  dygraphOutput("balance_tdplot"),
  plotOutput("obplot"),
  dygraphOutput("balance_obplot")
)