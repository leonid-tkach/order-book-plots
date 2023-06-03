# order-book-plots

Web App: https://orderbookplots.sleepyapper.com/. **The servers operate exclusively during weekdays from 8 AM to 8 PM Eastern Standard Time (EST).**

The presentation about this project: https://docs.google.com/presentation/d/1i27WkaikonMWRYUc3aiEB2DqW2aQznqykuqXclO93ko/edit#slide=id.g1a7e7bd42c3_1_92

This project consists of two repositories. The first part of this project is here: https://github.com/leonid-tkach/order-book-plot-find

Technologies I used:

TECHS|I USED
---|---
|<img src="./pres/0python.png" width="50">|Python: finding obplots (connected components) and add them to log files.<br>*I used: pandas, os, regex, time, pandas, datetime, matplotlib.*
|<img src="./pres/0R.png" width="50">|R: some extra data tidying, drawing plots.<br>*I used: tidyverse (ggplot, dplyr, tidyr), shiny, DBI, RPostgres, pool, dygrpahs, DT.*
|<img src="./pres/0RShiny.png" width="50">|R Shiny: web application.
|<img src="./pres/0AWS.png" width="50">|Amazon Web Services: deploying my web application.<br>*I used: EC2 (for Shiny Server), RDS (for PostgreSQL), Route 53 (to register domain).*
|<img src="./pres/0Postgresql.png" width="50">|PostgreSQL: database.<br>*I used: pgAdmin, Windows PowerShell, TablePlus.*
|<img src="./pres/0ubuntu.png" width="50">|Ubuntu: EC2 operating system.<br>*I use PuTTy terminal to admin it.*
|<img src="./pres/0NGINX.png" width="50">|F5 NGINX: web server.
|<img src="./pres/0let-s-encrypt.png" width="50">|Let's Encrypt: to get certificates to use HTTPS.
