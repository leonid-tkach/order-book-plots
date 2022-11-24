cursec = "LKOH"
curdate = "2007-10-08"
# cursec = "sec1"
# curdate = "2007-10-01"
# curplotno = 302
# curplotno = 1869
curplotno = 52
# curplotno = 2571
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
# browser()

function(input, output, session) {
  plot_df <- reactive({
    
    pbegin <- obp_cum_atts_enh_pg %>%
      filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
      pull(obpbegin)

    pend <- obp_cum_atts_enh_pg %>%
      filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
      pull(obpend)
    
    pmintprice <- obp_cum_atts_enh_pg %>%
      filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
      pull(obpmintradeprice)
    
    pmaxtprice <- obp_cum_atts_enh_pg %>%
      filter(seccode == cursec & ddate == curdate & obplotno == curplotno) %>% 
      pull(obpmaxtradeprice)
    
    plot_df <- order_atts_cumsums_enh_pg %>% 
      # filter(seccode == "LKOH" & ddate == "2007-10-08" & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL") & price > 2145.0 & price < 2205.0) %>% 
      filter(seccode == cursec & ddate == curdate & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL") & price >= pmintprice & price <= pmaxtprice) %>% 
      as_tibble()
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
  
  balance_df <- reactive({
    # browser()
    bal_df <- plot_df() %>% select(nno, 
                                                   # max_std_btd, minus_max_std_btd, 
                                                   sobp, bobp,
                                                   max_sobp_bobp, minus_max_sobp_bobp,
                                                   # stday, btday, 
                                                   obplotno)
    bal_df[bal_df$obplotno != curplotno,
           c("max_sobp_bobp", "minus_max_sobp_bobp", "sobp", "bobp")] <- NA
    bal_df %>% 
      select(-obplotno) %>%
      fill(sobp, bobp, max_sobp_bobp, minus_max_sobp_bobp)
  })

  output$obp_plot <- renderPlot({
    ggplot() +
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
  })

  output$balance_plot <- renderDygraph({
    # browser()
    dygraph(balance_df()) %>%
      dyOptions(fillGraph=TRUE, 
                colors = c(#"grey", "grey", 
                           "coral", "green", 
                           "grey", "grey"), 
                           # "red", "darkgreen"),
                fillAlpha = 1.0) %>% 
      dyLegend(show = c("always"))
  })
    
}