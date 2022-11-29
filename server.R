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
    # obplots_df()[1, 1] %>% 
    #   unlist() %>% 
    #   .[["obplotno"]]
    # browser()
    selected <- input$obplots_rtbl_rows_selected
    # browser()
    if(length(selected)) {
      return(
        isolate(
          obplots_df()[selected, "obplotno"] %>% 
            unlist() %>% 
            .[["obplotno"]]))
    }
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
  
  output$obplots_rtbl <- renderDT({
    # browser()
    datatable(obplots_df() %>% 
      select(-obpshareintd, -tradesnotrades, -seccode, -ddate,
             -obpbegin, -obpend),
      options = list(pageLength = 5),
      selection = list(mode = "single",
                       selected = 1))
  })
  
  pbegin <- reactive({
    # browser()
    req(obplots_df(), cur_ticker(), cur_date(), cur_obplotno())
    c_t <- cur_ticker()
    c_d <- cur_date()
    c_obpn <- cur_obplotno()
    # browser()
    obplots_df() %>%
      filter(seccode == c_t & ddate == c_d & obplotno == c_obpn) %>% 
      .$obpbegin
  })
  
  pend <- reactive({
    # browser()
    req(obplots_df(), cur_ticker(), cur_date(), cur_obplotno())
    c_t <- cur_ticker()
    c_d <- cur_date()
    c_obpn <- cur_obplotno()
    obplots_df() %>%
      filter(seccode == c_t & ddate == c_d & obplotno == c_obpn) %>% 
      .$obpend
  })
  
  # output$for_plot <- renderPrint({
  #   paste(pbegin(), pend())
  # })
  
  obp_plot_df <- reactive({
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
  
  obp_s <- reactive({
    # browser()
    req(obp_plot_df(), cur_obplotno())
    obp_plot_df() %>% filter(obplotno != cur_obplotno() & att == "SOVOL")
  })
  
  obp_b <- reactive({
    req(obp_plot_df(), cur_obplotno())
    obp_plot_df() %>% filter(obplotno != cur_obplotno() & att == "BOVOL")
  })
  
  obp_t <- reactive({
    req(obp_plot_df(), cur_obplotno())
    obp_plot_df() %>% filter(obplotno != cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  obp_cp_sb <- reactive({
    req(obp_plot_df(), cur_obplotno())
    obp_plot_df() %>% filter(obplotno == cur_obplotno() & att != "BTVOL" & att != "STVOL")
  })
  
  obp_cp_t <- reactive({
    req(obp_plot_df(), cur_obplotno())
    obp_plot_df() %>% filter(obplotno == cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  # dt_minmax_tprice <- reactive({
  #   plot_df() %>% 
  #     filter(att == "BTVOL" | att == "STVOL") %>% 
  #     mutate(mintprice = cummin(tradeprice), maxtprice = cummax(tradeprice)) %>% 
  #     select(nno, mintprice, maxtprice)
  # })
  
  balance_df <- reactive({
    # browser()
    req(obp_plot_df())
    bal_df <- obp_plot_df() %>% select(nno, datetimemlls,
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
    req(obp_s(), obp_b(), obp_t(), obp_cp_sb(), obp_cp_t())
    plot <- ggplot() +
      geom_point(data = obp_s(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = obp_s()$pcolor, shape = obp_s()$pshape, size = obp_s()$psize) +
      geom_point(data = obp_b(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = obp_b()$pcolor, shape = obp_b()$pshape, size = obp_b()$psize) +
      geom_point(data = obp_t(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = obp_t()$pcolor, shape = obp_t()$pshape, size = obp_t()$psize) +
      geom_point(data = obp_cp_sb(), mapping = aes(x = nno, y = price),
                 color = obp_cp_sb()$pcolor, shape = obp_cp_sb()$pshape, size = obp_cp_sb()$psize) +
      geom_point(data = obp_cp_t(), mapping = aes(x = nno, y = price),
                 color = obp_cp_t()$pcolor, shape = obp_cp_t()$pshape, size = obp_cp_t()$psize) +
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