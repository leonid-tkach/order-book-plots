library(shiny)
library(tidyverse)

fluidPage(
  "order-book-plots",
  plotOutput("obp_plot")
)