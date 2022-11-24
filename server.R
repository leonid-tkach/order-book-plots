cursec = "LKOH"
curdate = "2007-10-08"
# cursec = "sec1"
# curdate = "2007-10-01"
# curplotno = 302
# curplotno = 1869
# curplotno = 52
curplotno = 2571
# curplotno = 2452
# curplotno = 2482
# curplotno = 0

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

# order_atts_cumsums_enh_pg <- pool %>% tbl("order_atts_cumsums_enh4")
# obp_cum_atts_enh_pg <- pool %>% tbl("obp_cum_atts_enh")

order_atts_cumsums_enh_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/order_atts_cumsums_enh4_df.csv")
obp_cum_atts_enh_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/obp_cum_atts_enh_df.csv")

pbegin <- obp_cum_atts_enh_pg %>%
  filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
  pull(obpbegin)

pend <- obp_cum_atts_enh_pg %>%
  filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
  pull(obpend)

# browser()

function(input, output, session) {
  plot_df <- reactive({
    
    plot_df <- order_atts_cumsums_enh_pg %>% 
      # filter(seccode == "LKOH" & ddate == "2007-10-08" & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL") & price > 2145.0 & price < 2205.0) %>% 
      filter(seccode == cursec & ddate == curdate & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL")) %>% # & price >= pmintprice & price <= pmaxtprice) %>% 
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
    
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "BOVOL", "pcolor"] <- "darkgreen"
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "SOVOL", "pcolor"] <- "red"
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "BTVOL", "pcolor"] <- "#8031A7"
    # plot_df[plot_df$obplotno == curplotno, "pshape"] <- 16
    plot_df[, "pshape"] <- 16
    plot_df[, "psize"] <- 1.0
    plot_df[plot_df$obplotno == curplotno, "psize"] <- 2.0
    plot_df[plot_df$obplotno != curplotno & plot_df$att == "BOVOL", "pcolor"] <- "green"
    plot_df
  })

  dt_s <- reactive({
    plot_df() %>% filter(obplotno != curplotno & att == "SOVOL")
  })
  
  dt_b <- reactive({
    plot_df() %>% filter(obplotno != curplotno & att == "BOVOL")
  })

  dt_t <- reactive({
    plot_df() %>% filter(obplotno != curplotno & (att == "BTVOL" | att == "STVOL"))
  })
  
  dt_cp_sb <- reactive({
    plot_df() %>% filter(obplotno == curplotno & att != "BTVOL" & att != "STVOL")
  })
  
  dt_cp_t <- reactive({
    plot_df() %>% filter(obplotno == curplotno & (att == "BTVOL" | att == "STVOL"))
  })
  
  # dt_minmax_tprice <- reactive({
  #   plot_df() %>% 
  #     filter(att == "BTVOL" | att == "STVOL") %>% 
  #     mutate(mintprice = cummin(tradeprice), maxtprice = cummax(tradeprice)) %>% 
  #     select(nno, mintprice, maxtprice)
  # })
  
  balance_df <- reactive({
    # browser()
    bal_df <- plot_df() %>% select(nno, datetimemlls,
                                   sobp, bobp,
                                   max_sobp_bobp, minus_max_sobp_bobp,
                                   stday, btday,
                                   max_std_btd, minus_max_std_btd,
                                   obplotno)
    bal_df[bal_df$obplotno != curplotno,
           c("sobp", "bobp", "max_sobp_bobp", "minus_max_sobp_bobp", "stday", "btday",
             "max_std_btd", "minus_max_std_btd")] <- NA
    bal_df[bal_df$datetimemlls > pend,
           c("sobp", "bobp", "max_sobp_bobp", "minus_max_sobp_bobp")] <- 0.0
    bal_df %>% 
      select(-obplotno, -datetimemlls) %>%
      fill(sobp, bobp, max_sobp_bobp, minus_max_sobp_bobp, stday, btday, 
           max_std_btd, minus_max_std_btd)
  })

  output$obplot <- renderPlot({
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
    # browser()
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