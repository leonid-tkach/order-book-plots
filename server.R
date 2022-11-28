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
  
  dates_l <- reactive({
    order_atts_cumsums_pg %>%
      pull(ddate) %>%
      unique() %>%
      as.character() %>% 
      as.list()
  })
  
  obplots_df <- reactive({
    # browser()
    obplots_df <- obp_cum_atts_pg %>% 
      select(obplotno, obpshareintd, obpbegin, obpend, 
             tradesnotrades, seccode, ddate) %>% 
      as_tibble() %>% 
      filter(tradesnotrades == "T") %>% 
      filter(seccode == cur_ticker(),
             ddate == cur_date())
    obplots_df %>% 
      mutate(obpbegin = format(obpbegin, format = "%H:%M:%S"), 
             obpend = format(obpend, format = "%H:%M:%S"),
             share_in_td_vol = sprintf("%1.2f%%", 100*obpshareintd)) %>% 
      select(obplotno, share_in_td_vol, obpbegin, obpend, 
             -obpshareintd, -tradesnotrades, -seccode, -ddate)
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
  
  cur_ticker <- reactive({
    input$tickers_rb
  })
  
  observeEvent(dates_l, {
    updateRadioButtons(session, "dates_rb", "Choose date:", 
                       choiceNames = dates_l(), 
                       choiceValues = dates_l())
  })
  
  cur_date <- reactive({
    input$dates_rb
  })
  
  output$obplots_rtbl <- renderReactable({
    reactable(obplots_df(), defaultPageSize = 5)
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