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
  # order_atts_cumsums_pg <- pool %>% tbl("order_atts_cumsums")
  # obp_cum_atts_pg <- pool %>% tbl("obp_cum_atts")
  order_atts_cumsums_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/order_atts_cumsums_enh4_df.csv")
  obp_cum_atts_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/obp_cum_atts_enh_df.csv")
  
  tickers_l <- reactive({
    obp_cum_atts_pg %>%
      pull(seccode) %>%
      unique() %>%
      sort(decreasing = TRUE) %>% 
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
      select(obplotno, buysellobp, obpshareintd, obpbegin, obpend, 
             obpbeginno, obpendno,
             obpmintradeprice, obpmaxtradeprice,
             tradesnotrades, seccode, ddate) %>% 
      as_tibble() %>% 
      filter(tradesnotrades == "Y") %>% 
      filter(seccode == cur_ticker(),
             ddate == cur_date())
    obplots_df %>% 
      mutate(begin = format(obpbegin, format = "%H:%M:%S"), 
             end = format(obpend, format = "%H:%M:%S"),
             share_in_td_vol = sprintf("%1.2f%%", 100*obpshareintd)) %>% 
      select(obplotno, buysellobp, share_in_td_vol, begin, end, 
             obpbegin, obpend, 
             obpbeginno, obpendno,
             obpmintradeprice, obpmaxtradeprice,
             obpshareintd, 
             tradesnotrades, seccode, ddate)
  })
  
  cur_obplotno <- reactive({
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
    obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpbegin
  })
  
  pend <- reactive({
    # browser()
    req(obplots_df(), cur_ticker(), cur_date(), cur_obplotno())
    obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpend
  })
  
  tdmintprice <- 0
  tdmaxtprice <- 0
  obpmintradeprice <- 0
  obpmaxtradeprice <- 0
  obpbeginno <- 0
  obpendno <- 0
  td_plot_df <- reactive({
    # browser()
    req(cur_ticker(), cur_date(), pbegin(), pend())
    td_plot_df <- order_atts_cumsums_pg %>% 
      filter(seccode == cur_ticker() & ddate == cur_date() & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL")) %>%
      as_tibble()
    # browser()
    tdmintprice <<- min(obplots_df()$obpmintradeprice)
    tdmaxtprice <<- max(obplots_df()$obpmaxtradeprice)
    
    obpmintradeprice <<- obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpmintradeprice
    obpmaxtradeprice <<- obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpmaxtradeprice
    
    obpbeginno <<- obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpbeginno
    obpendno <<- obplots_df() %>%
      filter(seccode == cur_ticker() & ddate == cur_date() & obplotno == cur_obplotno()) %>% 
      .$obpendno
    
    td_plot_df[td_plot_df$obplotno == cur_obplotno() & td_plot_df$att == "BOVOL", "pcolor"] <- "darkgreen"
    td_plot_df[td_plot_df$obplotno == cur_obplotno() & td_plot_df$att == "SOVOL", "pcolor"] <- "red"
    td_plot_df[td_plot_df$obplotno == cur_obplotno() & td_plot_df$att == "BTVOL", "pcolor"] <- "#8031A7"
    td_plot_df[, "pshape"] <- 16
    td_plot_df[, "psize"] <- 1.0
    td_plot_df[td_plot_df$obplotno == cur_obplotno(), "psize"] <- 2.0
    td_plot_df[td_plot_df$obplotno != cur_obplotno() & td_plot_df$att == "BOVOL", "pcolor"] <- "green"
    td_plot_df
  })
  
  td_s <- reactive({
    # browser()
    req(td_plot_df(), cur_obplotno())
    td_plot_df() %>% filter(obplotno != cur_obplotno() & att == "SOVOL")
  })
  
  td_b <- reactive({
    req(td_plot_df(), cur_obplotno())
    td_plot_df() %>% filter(obplotno != cur_obplotno() & att == "BOVOL")
  })
  
  td_t <- reactive({
    req(td_plot_df(), cur_obplotno())
    td_plot_df() %>% filter(obplotno != cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  td_cp_sb <- reactive({
    req(td_plot_df(), cur_obplotno())
    td_plot_df() %>% filter(obplotno == cur_obplotno() & att != "BTVOL" & att != "STVOL")
  })
  
  td_cp_t <- reactive({
    req(td_plot_df(), cur_obplotno())
    td_plot_df() %>% filter(obplotno == cur_obplotno() & (att == "BTVOL" | att == "STVOL"))
  })
  
  
  td_balance_df <- reactive({
    # td_cp_t() %>% write_csv("../order-book-plot-find/cum_errors/resources/for_web_app/td_cp_t.csv")
    # browser()
    req(td_plot_df())
    bal_df <- td_plot_df() %>% select(nno, datetimemlls,
                                      sobp, bobp,
                                      max_sobp_bobp, minus_max_sobp_bobp,
                                      stday, btday,
                                      max_std_btd, minus_max_std_btd,
                                      obplotno)
    # browser()
    bal_df[bal_df$obplotno != cur_obplotno(),
           c("sobp", "bobp", 
             "max_sobp_bobp", "minus_max_sobp_bobp")] <- NA
    bal_df <- bal_df %>% 
      select(-obplotno, -datetimemlls)
    bal_df <- bal_df %>% fill(sobp, bobp, 
                              max_sobp_bobp, minus_max_sobp_bobp)
    # browser()
    bal_df
  })
  
  output$obplot <- renderPlot({
    # browser()
    req(td_s(), td_b(), td_t(), td_cp_sb(), td_cp_t())
    plot <- ggplot() +
      geom_point(data = td_s(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_s()$pcolor, shape = td_s()$pshape, size = td_s()$psize) +
      geom_point(data = td_b(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_b()$pcolor, shape = td_b()$pshape, size = td_b()$psize) +
      geom_point(data = td_t(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_t()$pcolor, shape = td_t()$pshape, size = td_t()$psize) +
      geom_point(data = td_cp_sb(), mapping = aes(x = nno, y = price),
                 color = td_cp_sb()$pcolor, shape = td_cp_sb()$pshape, size = td_cp_sb()$psize) +
      geom_point(data = td_cp_t(), mapping = aes(x = nno, y = price),
                 color = td_cp_t()$pcolor, shape = td_cp_t()$pshape, size = td_cp_t()$psize) +
      scale_x_continuous(expand = c(0, 0)) +
      xlim(obpbeginno, obpendno) +
      ylim(obpmintradeprice, obpmaxtradeprice) +
      theme_bw()
    
    plot +
      theme(plot.margin = margin(0, 0, 0, 1, "cm"))
  })
  
  output$balance_obplot <- renderDygraph({
    req(td_balance_df())
    dygraph(td_balance_df() %>% 
              select(-stday, -btday,
                     -max_std_btd, -minus_max_std_btd) %>% 
              filter(nno >= obpbeginno & nno <= obpendno)) %>%
      dyOptions(fillGraph=TRUE, 
                colors = c("red", "darkgreen", 
                           "gray", "gray"), 
                           # "coral", "green",
                           # "silver", "silver"),
                fillAlpha = 1.0) %>% 
      dyLegend(show = c("always")) %>% 
      dyCSS("dygraph.css")
  })

  output$tdplot <- renderPlot({
    # browser()
    req(td_s(), td_b(), td_t(), td_cp_sb(), td_cp_t())
    plot <- ggplot() +
      geom_point(data = td_s(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_s()$pcolor, shape = td_s()$pshape, size = td_s()$psize) +
      geom_point(data = td_b(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_b()$pcolor, shape = td_b()$pshape, size = td_b()$psize) +
      geom_point(data = td_t(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = td_t()$pcolor, shape = td_t()$pshape, size = td_t()$psize) +
      geom_point(data = td_cp_sb(), mapping = aes(x = nno, y = price),
                 color = td_cp_sb()$pcolor, shape = td_cp_sb()$pshape, size = td_cp_sb()$psize) +
      geom_point(data = td_cp_t(), mapping = aes(x = nno, y = price),
                 color = td_cp_t()$pcolor, shape = td_cp_t()$pshape, size = td_cp_t()$psize) +
      scale_x_continuous(expand = c(0, 0)) +
      ylim(tdmintprice, tdmaxtprice) +
      theme_bw()
    
    plot +
      theme(plot.margin = margin(0, 0, 0, 1, "cm"))
  })
  
  output$balance_tdplot <- renderDygraph({
    req(td_balance_df())
    dygraph(td_balance_df()) %>%
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