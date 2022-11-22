library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(pool)
library(dygraphs)

fluidPage(
  "order-book-plots",
  plotOutput("obp_plot"),
  dygraphOutput("balance_plot")
)