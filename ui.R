library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(pool)
library(dygraphs)
library(reactable)

fluidPage(
  "order-book-plots",
  fluidRow(
    column(3,
           radioButtons("tickers_rb", "Choose ticker:", "")),
    column(3,
           radioButtons("dates_rb", "Choose date:", "")),
    column(6,
           tags$b("Choose order-book plot:"),
           reactableOutput("obplots_rtbl"))
  ),
  fluidRow(
    column(3,
           verbatimTextOutput("cur_ticker")),
    column(3,
           verbatimTextOutput("cur_date")),
    column(6,
           verbatimTextOutput("cur_obplotno"))
  )
)