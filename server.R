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
  
  tickers_l <- reactive({
    obp_cum_atts_pg %>%
      pull(seccode) %>%
      unique() %>%
      as.list()
  })
  
  cur_ticker <- reactive({
    tickers_l()[1] %>% unlist()
  })
  
  dates_l <- reactive({
    order_atts_cumsums_pg %>%
      pull(ddate) %>%
      unique() %>%
      as.character() %>% 
      as.list()
  })
  
  cur_date <- reactive({
    # browser()
    dates_l()[1] %>% 
      unlist()
  })
  
  obplots_df <- reactive({
    obp_cum_atts_pg %>% 
      select(obplotno, obpshareintd) %>% 
      as_tibble()
  })
  
  cur_obplotno <- reactive({
    obplots_df()[1, 1] %>% 
      unlist() %>% 
      .[["obplotno"]]
  })
  
  observeEvent(tickers_l, {
    updateRadioButtons(session, "tickers_rb", "Choose ticker:", 
                       choiceNames = tickers_l(), 
                       choiceValues = tickers_l())
  })
  
  observeEvent(dates_l, {
    updateRadioButtons(session, "dates_rb", "Choose date:", 
                       choiceNames = dates_l(), 
                       choiceValues = dates_l())
  })
  
  output$obplots_rtbl <- renderReactable({
    reactable(obplots_df())
  })
  
  output$cur_ticker <- renderPrint({
    cur_ticker()
  })
  
  output$cur_date <- renderPrint({
    cur_date()
  })

  output$cur_obplotno <- renderPrint({
    cur_obplotno()
  })
  
}