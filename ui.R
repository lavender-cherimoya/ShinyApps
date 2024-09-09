fluidPage(
      titlePanel('Diabetes Prediction Application'),
      
      sidebarLayout(
            sidebarPanel(
                  h4("Input Parameters"),
                  selectInput("xvar", "X-axis variable", choices = names(dataset)[1:8],
                              selected = names(dataset)[8]),
                  selectInput("yvar", "Y-axis variable", choices = names(dataset)[1:8],
                              selected = names(dataset)[2]),
                  sliderInput("numPoints", "Number of points to display:", 
                              min = 30, max = 100, value = 30, step = 10),
                  br(),
                  actionButton("show_doc", "Documentation"),
                  #actionButton("predict", "Predict Diabetes Status"),
                  tags$hr(),
                  h4("Prediction Results"),
                  uiOutput("prediction"),
                  #actionButton("toggle_true_answer", "Show True Answer")
                  
                  
            ),
            
            mainPanel(
                  plotlyOutput("scatterPlot"),
                  uiOutput("selectSample"),
            )
      )
      
      
)