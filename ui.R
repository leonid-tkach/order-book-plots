library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(pool)

fluidPage(
  "order-book-plots",
  plotOutput("obp_plot"),
  plotOutput("balance_plot")
)