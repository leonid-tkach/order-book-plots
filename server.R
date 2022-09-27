function(input, output, session) {
  clustered_obp_atts_df <- read.csv('./resources/clustered_obp_atts_df.csv')
  output$obp_atts_plot <- renderPlotly({
    plot_ly(
      type = "scatter3d",
      mode = "markers",
      data = clustered_obp_atts_df,
      x = ~BUYSELLYIELD,
      y = ~OBPTDVOLRATIO,
      z = ~MINMAXRATIO,
      color = ~GROUP
    )
  })
}