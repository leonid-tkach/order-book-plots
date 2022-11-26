library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(pool)
library(dygraphs)

fluidPage(
  "order-book-plots",
  plotOutput("obplot"),
  dygraphOutput("balance_obplot")
)