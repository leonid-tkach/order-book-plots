# curplotno = 302
# curplotno = 1869
# curplotno = 52
curplotno = 0
# curplotno = 3

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
  plot_df <- reactive({
    
    # order_atts_cumsums_enh_pg <- pool %>% tbl("order_atts_cumsums_enh4")
    # obp_cum_atts_enh_pg <- pool %>% tbl("obp_cum_atts_enh")
    
    order_atts_cumsums_enh_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/order_atts_cumsums_enh4_df.csv")
    obp_cum_atts_enh_pg <- read_csv("../order-book-plot-find/cum_errors/resources/for_web_app/obp_cum_atts_enh_df.csv")
    # browser()
    
    pbegin <- obp_cum_atts_enh_pg %>%
      # filter(seccode == "LKOH" & ddate == "2007-10-08" & obplotno == curplotno) %>% 
      filter(seccode == "sec1" & ddate == "2007-10-01" & obplotno == curplotno) %>% 
      pull(obpbegin)
    
    pend <- obp_cum_atts_enh_pg %>%
      # filter(seccode == "LKOH" & ddate == "2007-10-08" & obplotno == curplotno) %>% 
      filter(seccode == "sec1" & ddate == "2007-10-01" & obplotno == curplotno) %>% 
      pull(obpend)
    
    plot_df <- order_atts_cumsums_enh_pg %>% 
      # filter(seccode == "LKOH" & ddate == "2007-10-08" & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL") & price > 2145.0 & price < 2205.0) %>% 
      filter(seccode == "sec1" & ddate == "2007-10-01" & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL")) %>% 
      as_tibble()
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "BOVOL", "pcolor"] <- "green"
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "SOVOL", "pcolor"] <- "red"
    plot_df[plot_df$obplotno == curplotno & plot_df$att == "BTVOL", "pcolor"] <- "#8031A7"
    plot_df[plot_df$obplotno == curplotno, "pshape"] <- 16
    plot_df[plot_df$obplotno == curplotno, "psize"] <- 1.0
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

  output$obp_plot <- renderPlot({
    ggplot(bind_rows(tibble(dt_s(), gr = "s"), tibble(dt_b(), gr = "b")), aes(x = nno, y = price)) +
      stat_density2d(geom = "density2d", aes(color = gr, alpha = ..level..)) +
      scale_color_manual(values=c("s"="#FF0000", "b"="#00FF00")) +
      geom_point(data = dt_t(), mapping = aes(x = nno, y = price),# alpha = val),
                 color = dt_t()$pcolor, shape = dt_t()$pshape, size = dt_t()$psize) +
      geom_point(data = dt_cp_sb(), mapping = aes(x = nno, y = price),
                 color = dt_cp_sb()$pcolor, shape = dt_cp_sb()$pshape, size = dt_cp_sb()$psize) +
      geom_point(data = dt_cp_t(), mapping = aes(x = nno, y = price),
                 color = dt_cp_t()$pcolor, shape = dt_cp_t()$pshape, size = dt_cp_t()$psize) +
      theme_bw()
  })

  output$balance_plot <- renderPlot({
    ggplot(plot_df()) +
      geom_line(aes(x=nno, y=bovoltdcs)) +
      geom_line(aes(x=nno, y=bovolobpcs)) +
      geom_line(aes(x=nno, y=-sovolobpcs)) +
      geom_line(aes(x=nno, y=-sovoltdcs)) +
      geom_line(aes(x=nno, y=btvoltdcs)) +
      geom_line(aes(x=nno, y=btvolobpcs)) +
      geom_line(aes(x=nno, y=-stvolobpcs)) +
      geom_line(aes(x=nno, y=-stvoltdcs)) +
      # scale_y_log10() +
      theme_bw()
  })
    
}