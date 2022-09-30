library(shiny)
# library(tidyverse)
library(plotly)

# When we run nginx we need to change R to point to the /app folder manually
setwd("/app")

# Create a file at the path /tmp/app-initalized
# This tells nginx that the app is running, and it can now forward traffic to the custom 'port' below
# If this file is created, nginx will simply wait forever until the app says its running
file.create("/tmp/app-initialized")
port <- "/tmp/nginx.socket"
attr(port, "mask") <- strtoi("117", 8)


# Runs the app in the working directory. By default this will search for an `app.R` file or
# a ui.R + server.R file combination
# In this case the port is now the address to the nginx proxy
shiny::runApp(
  appDir = getwd(),
  port = port
)


# # Gets the port # from Heroku
# port <- Sys.getenv("PORT")
# 
# # Runs the app in the working directory. By default this will search for an `app.R` file or
# # a ui.R + server.R file combination
# # The host of 0.0.0.0 indicates the app runs at the web server's ip address
# shiny::runApp(
#   appDir = getwd(),
#   host = "0.0.0.0",
#   port = as.numeric(port)
# )