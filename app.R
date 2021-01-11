library(dplyr)
library(shiny)
library(ggplot2)
options(scipen = 999)

options(shiny.maxRequestSize = 30*1024^2)
# 30MB
Palette <- c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", "#B15928", "#A6CEE3", 
             "#B2DF8A", "#FB9A99", "#FDBF6F", "#CAB2D6", "#FFFF99", "#999999")

# server.R
server <- function(input, output, session) {
  #read file
   data <- reactive({
    file1 <- input$file1
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, header =TRUE, stringsAsFactors = FALSE)
  })
   
    output$plot1 <- renderPlot({
      if (is.null(data())) 
        return (NULL)
      ex_df <- data()
      ex_df$Ancestry[grep(";",ex_df$Ancestry)] <- "Missing"
      uniq.grp <- unique(ex_df$Ancestry)
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
      
    ggplot(ex_df, aes(PC1, PC2, color=Ancestry)) + geom_point(aes(colour = factor(Ancestry, levels = legend.group))) +
      scale_colour_manual(values=cols) 
  })

} 

# ui.R
ui <- fluidPage( 
  titlePanel(h4("Input Information")),
  sidebarLayout(position = "left",
                sidebarPanel( 
                  fileInput("file1", "Choose txt File",accept = "text"),
                  width = 2
                ),
                mainPanel(
                  
                  fluidRow(
                                       plotOutput(outputId = "plot1", width = "100%")),
                  fluidRow(
                    column(width = 8,
                           verbatimTextOutput("click_info"),
                           verbatimTextOutput("last_infor"))))
  )
)

shinyApp(ui = ui, server = server)
