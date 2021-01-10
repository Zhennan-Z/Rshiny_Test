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
