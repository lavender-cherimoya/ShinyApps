fluidPage(
      # Add a title
      titlePanel('Diabetes Prediction Application'),
      
      # Add a sidebar
      sidebarLayout(
            sidebarPanel(
                  # Add two menus to select the x and the y parameters
                  h4("Input Parameters"),
                  selectInput("xvar", "X-axis variable", choices = names(dataset)[1:8],
                              selected = names(dataset)[8]),
                  selectInput("yvar", "Y-axis variable", choices = names(dataset)[1:8],
                              selected = names(dataset)[2]),
                  # Add a slider to select how many data point to display on the graphic
                  sliderInput("numPoints", "Number of points to display:", 
                              min = 30, max = 100, value = 30, step = 10),
                  br(),
                  # Add an action button for the documentation
                  actionButton("show_doc", "Documentation"),
                  # Add a section where the prediction results will be displayed
                  tags$hr(),
                  h4("Prediction Results"),
                  uiOutput("prediction"),
                  
                  
            ),
            
            # The main panel will display the interactive scatterplot where a
            # data point can be selected
            mainPanel(
                  plotlyOutput("scatterPlot"),
                  uiOutput("selectSample"),
            )
      )
      
      
)