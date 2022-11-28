pool  <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("dbname"),
  host = Sys.getenv("host"),
  # port = Sys.getenv("port"),
  user = Sys.getenv("user"),
  password = Sys.getenv("password")
)

onStop(function() {
  # message("before close - is valid? ", DBI::dbIsValid(pool))
  poolClose(pool)
  # message("after close - is valid? ", DBI::dbIsValid(pool))
})

function(input, output, session) {
  order_atts_cumsums_pg <- pool %>% tbl("order_atts_cumsums")
  obp_cum_atts_pg <- pool %>% tbl("obp_cum_atts")
  tickers <- obp_cum_atts_pg %>%
    pull(seccode) %>%
    unique() %>%
    as.list()
  # browser()
  output$tickers <- renderUI({
    radioButtons("tickers_rb", "Choose ticker:", 
                 choiceNames = tickers, 
                 choiceValues = tickers)
  })
}