function(input, output, session) {
  clustered_obp_atts_df <- read.csv('./resources/5_obp_grouped_by_atts_df.csv')
  output$obp_atts_plot <- renderPlot({
    ggplot(bind_rows(tibble(dt_s, gr = "s"), tibble(dt_b, gr = "b")), aes(x = NO, y = PRICE)) +
      stat_density2d(geom = "density2d", aes(color = gr)) + 
      scale_color_manual(values=c("s"="#FF0000", "b"="#00FF00")) +
      geom_point(data = dt_t, mapping = aes(x = NO, y = PRICE), 
                 color = dt_t$pcolor, shape = dt_t$pshape, size = dt_t$psize) +
      geom_point(data = dt_cp_sb, mapping = aes(x = NO, y = PRICE), 
                 color = dt_cp_sb$pcolor, shape = dt_cp_sb$pshape, size = dt_cp_sb$psize) +
      geom_point(data = dt_cp_t, mapping = aes(x = NO, y = PRICE), 
                 color = dt_cp_t$pcolor, shape = dt_cp_t$pshape, size = dt_cp_t$psize) +
      theme_bw()
  })
}