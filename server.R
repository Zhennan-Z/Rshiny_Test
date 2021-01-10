# server.R
server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    ggplot(ex_df, aes(PC1, PC2, color=Ancestry))  + geom_point(aes(colour = factor(Ancestry, levels = legend.group))) +
     scale_colour_manual(values=cols) 
  })
  observe({
    subset <- filter(ex_df, Ancestry==input$Ancestry)
    updateSliderInput(session, "slide", value = c(min(subset$PC1), max(subset$PC1)),min = min(subset$PC1), max = max(subset$PC1))
    updateSliderInput(session, "slide2", value = c(min(subset$PC2), max(subset$PC2)),min = min(subset$PC2), max = max(subset$PC2))
  })
  #PLOT 2
  output$plot2 <- renderPlot({
    labels <- sapply(legend.group, function(x) paste0(x, " (N=", sum(ex_df$Ancestry == x), ")"))
    ggplot(filter(ex_df, Ancestry==input$Ancestry & PC1 >= input$slide[1] & PC1 <= input$slide[2] & PC2 >= input$slide2[1] & PC2 <= input$slide2[2]), aes(PC1, PC2)) + 
      geom_point(color=cols[legend.group%in%input$Ancestry]) + 
      facet_wrap(~factor(Ancestry, labels=labels[legend.group%in%input$Ancestry]))
  })
    output$click_info <- renderPrint({
    if(!is.null(input$plot_click)){
      print(paste0("PC1 = ", round(input$plot_click$x,4), ";PC2 = ", round(input$plot_click$y,4)))
      sample.click <- ex_df[which.min((ex_df$PC1-input$plot_click$x)^2 + (ex_df$PC2-input$plot_click$y)^2), ]
      if (abs(sample.click$PC1-input$plot_click$x) <= 0.001 & abs(sample.click$PC2-input$plot_click$y) <= 0.001){
        print(sample.click )} else {
          print("Please click one dot")
        }
    }
  })
} 

 
