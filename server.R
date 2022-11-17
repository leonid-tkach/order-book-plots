# curplotno = 302
curplotno = 1869
# curplotno = 52

function(input, output, session) {
  # output$obp_plot <- renderPlot({
  #   ggplot(data = mpg) + 
  #     geom_point(mapping = aes(x = displ, y = hwy, color = class))
  # })

  # browser()
  plot_df <- reactive({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("dbname"),
      host = Sys.getenv("host"),
      port = Sys.getenv("port"),
      user = Sys.getenv("user"),
      password = Sys.getenv("password")
    )
    order_atts_cumsums_enh_pg <- tbl(con, "order_atts_cumsums_enh4")
    obp_cum_atts_enh_pg <- tbl(con, "obp_cum_atts_enh")

    pbegin <- obp_cum_atts_enh_pg %>%
      filter(seccode == "LKOH" & ddate == "2007-10-08" & obplotno == curplotno) %>% 
      pull(obpbegin)
    
    pend <- obp_cum_atts_enh_pg %>%
      filter(seccode == "LKOH" & ddate == "2007-10-08" & obplotno == curplotno) %>% 
      pull(obpend)
    
    plot_df <- order_atts_cumsums_enh_pg %>% 
      filter(seccode == "LKOH" & ddate == "2007-10-08" & (datetimemlls >= pbegin & datetimemlls <= pend) & (att == "BOVOL" | att == "SOVOL" | att == "BTVOL" | att == "STVOL") & price > 2145.0 & price < 2205.0) %>% 
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
}