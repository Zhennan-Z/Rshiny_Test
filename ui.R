ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Identical-By-Descent Segments")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             #fileInput("fileinfer", "Choose a *.seg file"),
                             #fileInput("fileibdseg", "Choose a *.segments.gz file"),
                             #fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             
                             actionButton(inputId = "filechoose", label = "Choose a *.seg file"),
                             fluidRow(
                               column(8,
                                      textInput(inputId = "FID",
                                                label = "Family ID (Optional)",
                                                value = "All")
                               )),
                             actionButton(inputId = "EnterFID", label = "Generate Plots"),
                             #actionButton(inputId = "EnterAllFID", label = "Clear"),
                             sliderInput("IBD1Seg", "IBD1Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("IBD2Seg", "IBD2Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             selectizeInput("IDs", "IDs",choices =c(Choose='')),
                             width = 2
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Main Plot", 
                             fluidRow(
                               splitLayout(style = "border: 1px solid silver:", 
                                           plotOutput(outputId = "plot1", click = "plot_click", height = "600px"),
                                           plotOutput(outputId = "plot2", height = "600px", width = "100%")
                               )),
                             fluidRow(
                               dataTableOutput(outputId = "dt1")
                             ),
                             fluidRow(
                               column(width = 5,
                                      verbatimTextOutput("click_info"),
                                      verbatimTextOutput("last_infor")))
                    ),
                    tabPanel("IBD Segments for the Selected Pair",
                             plotOutput("plot3",height = "600px", width = "80%"),
                             dataTableOutput(outputId = "dt2")
                    )
                    
                  )))
)
