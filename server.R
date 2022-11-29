options(digits.secs = 3)
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
      mutate(begin = format(obpbegin, format = "%H:%M:%S"), 
             end = format(obpend, format = "%H:%M:%S"),
             share_in_td_vol = sprintf("%1.2f%%", 100*obpshareintd)) %>% 
      select(obplotno, share_in_td_vol, begin, end, 
             obpbegin, obpend, obpshareintd, tradesnotrades, seccode, ddate)
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
    reactable(obplots_df() %>% 
                select(-obpshareintd, -tradesnotrades, -seccode, -ddate,
                       -obpbegin, -obpend), 
              defaultPageSize = 5)
  })
  
  output$cur_ticker <- renderPrint({
    cur_ticker()
  })
  
  output$cur_date <- renderPrint({
    cur_date()
  })

  output$cur_obplotno <- renderPrint({
    # browser()
    cur_obplotno()
  })
  
  pbegin <- reactive({
    # browser()
    obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpbegin
  })
  
  pend <- reactive({
    # browser()
    obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpend
  })
  
  # output$for_plot <- renderPrint({
  #   paste(pbegin(), pend())
  # })
  
  plot_df <- reactive({
    # browser()
    req(cur_ticker(), cur_date(), pbegin(), pend())
    c_t <- cur_ticker()
    c_d <- cur_date()
    pb <- pbegin()
    pe <- pend()
    plot_df <- order_atts_cumsums_pg %>% 
      filter(seccode == c_t & ddate == c_d & (datetimemlls >= pb & datetimemlls <= pe) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL")) %>%
      as_tibble()
    # browser()
    pmintprice <- min(plot_df %>%
                        filter(att == "BTVOL" | att == "STVOL") %>%
                        .$tradeprice %>%
                        cummin())
    pmaxtprice <- max(plot_df %>%
                        filter(att == "BTVOL" | att == "STVOL") %>%
                        .$tradeprice %>%
                        cummax())
    plot_df <- plot_df %>%
      filter(price >= pmintprice & price <= pmaxtprice)
    
    # plot_df <- plot_df %>%
    #   filter(price > 2145.0 & price < 2205.0)
    
    plot_df[plot_df$obplotno == cur_obplotno() & plot_df$att == "BOVOL", "pcolor"] <- "darkgreen"
    plot_df[plot_df$obplotno == cur_obplotno() & plot_df$att == "SOVOL", "pcolor"] <- "red"
    plot_df[plot_df$obplotno == cur_obplotno() & plot_df$att == "BTVOL", "pcolor"] <- "#8031A7"
    # plot_df[plot_df$obplotno == curplotno, "pshape"] <- 16
    plot_df[, "pshape"] <- 16
    plot_df[, "psize"] <- 1.0
    plot_df[plot_df$obplotno == cur_obplotno(), "psize"] <- 2.0
    plot_df[plot_df$obplotno != cur_obplotno() & plot_df$att == "BOVOL", "pcolor"] <- "green"
    plot_df
  })
  
  dt_s <- reactive({
    # browser()
    req(plot_df(), cur_obplotno())
    plot_df() %>% filter(obplotno != cur_obplotno() & att == "SOVOL")
  })
  
  dt_b <- reactive({
    req(plot_df(), cur_obplotno())
    plot_df() %>% filter(obplotno != cur_obplotno() & att == "BOVOL")
  })
  
  dt_t <- reactive({
    req(plot_df(), cur_obplotno())
    plot_df() %>% filter(obplotno != cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  dt_cp_sb <- reactive({
    req(plot_df(), cur_obplotno())
    plot_df() %>% filter(obplotno == cur_obplotno() & att != "BTVOL" & att != "STVOL")
  })
  
  dt_cp_t <- reactive({
    req(plot_df(), cur_obplotno())
    plot_df() %>% filter(obplotno == cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  # dt_minmax_tprice <- reactive({
  #   plot_df() %>% 
  #     filter(att == "BTVOL" | att == "STVOL") %>% 
  #     mutate(mintprice = cummin(tradeprice), maxtprice = cummax(tradeprice)) %>% 
  #     select(nno, mintprice, maxtprice)
  # })
  
  balance_df <- reactive({
    # browser()
    req(plot_df())
    bal_df <- plot_df() %>% select(nno, datetimemlls,
                                   sobp, bobp,
                                   max_sobp_bobp, minus_max_sobp_bobp,
                                   stday, btday,
                                   max_std_btd, minus_max_std_btd,
                                   obplotno)
    # browser()
    bal_df[bal_df$obplotno != cur_obplotno(),
           c("sobp", "bobp", "max_sobp_bobp", "minus_max_sobp_bobp", "stday", "btday",
             "max_std_btd", "minus_max_std_btd")] <- NA
    bal_df[bal_df$datetimemlls > pend(),
           c("sobp", "bobp", "max_sobp_bobp", "minus_max_sobp_bobp")] <- 0.0
    bal_df <- bal_df %>% 
      select(-obplotno, -datetimemlls)
    bal_df <- bal_df %>% fill(sobp, bobp, max_sobp_bobp, minus_max_sobp_bobp, stday, btday,
                              max_std_btd, minus_max_std_btd)
    # browser()
    bal_df
  })
  
  output$obplot <- renderPlot({
    # browser()
    req(dt_s(), dt_b(), dt_t(), dt_cp_sb(), dt_cp_t())
    plot <- ggplot() +
      geom_point(data = dt_s(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = dt_s()$pcolor, shape = dt_s()$pshape, size = dt_s()$psize) +
      geom_point(data = dt_b(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = dt_b()$pcolor, shape = dt_b()$pshape, size = dt_b()$psize) +
      geom_point(data = dt_t(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = dt_t()$pcolor, shape = dt_t()$pshape, size = dt_t()$psize) +
      geom_point(data = dt_cp_sb(), mapping = aes(x = nno, y = price),
                 color = dt_cp_sb()$pcolor, shape = dt_cp_sb()$pshape, size = dt_cp_sb()$psize) +
      geom_point(data = dt_cp_t(), mapping = aes(x = nno, y = price),
                 color = dt_cp_t()$pcolor, shape = dt_cp_t()$pshape, size = dt_cp_t()$psize) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_bw()
    
    plot +
      theme(plot.margin = margin(0, 0, 0, 1, "cm"))
  })
  
  output$balance_obplot <- renderDygraph({
    req(balance_df())
    dygraph(balance_df()) %>%
      dyOptions(fillGraph=TRUE, 
                colors = c("red", "darkgreen", 
                           "gray", "gray", 
                           "coral", "green",
                           "silver", "silver"),
                fillAlpha = 1.0) %>% 
      dyLegend(show = c("always")) %>% 
      dyCSS("dygraph.css")
  })
  
}