library(dplyr)
library(data.table)
library(shiny)
library(ggplot2)
options(scipen = 999)

# global.R
ex_df <- read.table("large_ex_Inference.txt", header = TRUE, stringsAsFactors = FALSE)
ex_df$Ancestry[grep(";", ex_df$Ancestry)] <- "Missing"

x.low <- min(ex_df$PC1)
x.high <- max(ex_df$PC1)
y.low <- min(ex_df$PC2)
y.high <- max(ex_df$PC2)

uniq.grp <- unique(ex_df$Ancestry)

# gg_color_hue <- function(n) {
# hues = seq(15, 375, length = n + 1)
#hcl(h = hues, l = 65, c = 100)[1:n]
#}


Palette <- c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", "#B15928", "#A6CEE3", 
             "#B2DF8A", "#FB9A99", "#FDBF6F", "#CAB2D6", "#FFFF99", "#999999")

if ("Missing" %in% uniq.grp ) {
legend.group <- c(sort(uniq.grp[uniq.grp != "Missing"]), "Missing") } else {
  legend.group <- sort(uniq.grp)
}

n = length(uniq.grp)
if ("Missing" %in% uniq.grp ) {
  cols=c(Palette[1:(n-1)],"#999999")
} else {
  cols = Palette[1:n]
  }

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

# ui.R
ui <- fluidPage( 
  titlePanel(h4("Input Information")),
  sidebarLayout(position = "left",
                sidebarPanel( 
                  selectInput(inputId = "Ancestry", label = "Ancestry", choices = legend.group),
                  sliderInput(inputId = "slide",label = "PC1 Range", min = x.low, max = x.high,value = c(x.low, x.high)),
                  sliderInput(inputId = "slide2",label = "PC2 Range", min = y.low,max = y.high,value = c(y.low,y.high)),
                  width = 2
                ),
                mainPanel(
                  
                  fluidRow(
                                       plotOutput(outputId = "plot1", width = "100%"),
                                       plotOutput(outputId = "plot2",width = "100%",click = "plot_click")),
                  fluidRow(
                    column(width = 8,
                           verbatimTextOutput("click_info"),
                           verbatimTextOutput("last_infor"))))
  )
)

shinyApp(ui = ui, server = server)
