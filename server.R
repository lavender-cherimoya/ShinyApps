function(input, output) {
      
      # Call the documentationModal() function, written in global.R, to display the
      # documentation text
      observeEvent(input$show_doc, {
            showModal(documentationModal())
      })
      
      # Text about the data, which is displayed when clicking on the Data Information
      # button, which is available in the Documentation page
      observeEvent(input$show_data_info, {
            showModal(modalDialog(
                  title = "Data Information",
                  p("The dataset used in this application is the PimaIndiansDiabetes 
                    dataset available in the `mlbench` R package. It is derived 
                    from the National Institude of Diabetes and Digestive and 
                    Kidney Diseases, and was originally used to study the diabetes
                    status of the Pima Indian population. The dataset contains the 
                    data of 768 patients, each patient being described with 9 features."),
                  p("Description of the parameters:"),
                  tags$ul(
                        tags$li("NumberPregnancies: The number of times the patient has been pregnant."),
                        tags$li("Glucose: The plasma glucose concentration, measured in mg/dL."),
                        tags$li("BloodPressure: The diastolic blood pressure, measured in mm Hg."),
                        tags$li("SkinThickness: The triceps skinfold thickness, measured in mm."),
                        tags$li("Insulin: The 2-hour serum insulin concentration, measured in µU/mL."),
                        tags$li("BMI: The body mass index, in kg/m^2."),
                        tags$li("PedigreeFunction: The diabetes pedigree function, which scores the likelihood of diabetes based on family history."),
                        tags$li("Age: The age of the patient in years."),
                  ),
                  p("Lastly, the diabetes variable describes if the patient has 
                    a positive or negative diabetes status. This parameter is used
                    as the target variable for the prediction part of the app, and 
                    is therefore not accessible."),
                  
                  footer = tagList(
                        actionButton("back_to_doc", "Back"),
                        modalButton("Close")
                  )
            ))
      })
      # When the user clicks on the Back document, display again the text from
      # the documentation function
      observeEvent(input$back_to_doc, {
            removeModal()
            showModal(documentationModal())
      })
      
      # Create the variable that will contain the clicked data point by the user
      clickedPoint <- reactiveVal(NULL)

      # Create a datasubset with the number of points given as input by the user
      sampleData <- reactive({
            data_subset <- dataset[sample(1:nrow(dataset), input$numPoints), ]
            data_subset
      })
    
      # Create the scatter plot with the sampleData
      output$scatterPlot <- renderPlotly({
          
            data <- sampleData()
            # Add a color parameter, to give different colors to the unselected points
            # (blue) and to the selected point (red)
            data$color <- "Unselected data points"
            clicked_idx <- clickedPoint()
          
            if(!is.null(clicked_idx)) {
                   data$color[clicked_idx] <- "Selected data point"
            }
            
            # Give the plot a title
            plot_title <- paste("Scatter plot of", input$yvar, "as a function of", 
                                input$xvar, "for", nrow(data), "patients")
          
            # Plot the scatter plot as a ggplot
            p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
                  geom_point(aes(color = color), size=2.5, shape=21, stroke=1, 
                              alpha=0.7) +
                  scale_color_manual(values = c("Selected data point" = "salmon", "Unselected data points" = "lightblue"),
                                     name = "Data Points") +
                  theme_minimal() +
                  ggtitle(plot_title)
          
            # Transform it into an interactive plotly plot
            ggplotly(p) %>%
                  layout(hovermode="closest")
      })
    
      selectSample <- reactive({
            # Save the data of the clicked point
            click_data <- event_data("plotly_click")
          
            #print("Select Sample Triggered") # Debugging
          
            if (is.null(click_data)) {
                  return(NULL)
            }
          
            data <- sampleData()
          
            # Get the exact row of the clicked data point
            clicked_row <- which(data[[input$xvar]] == click_data$x &
                                     data[[input$yvar]] == click_data$y)
          
            # Save the row in question, if existing, in the clickedPoint variable 
            # and in the selected variable
            if (length(clicked_row) == 1) {
                  clickedPoint(clicked_row)
                  selected <- data[clicked_row, ]
                  return(selected)
            } else {
                  return(NULL)
            }
      })

    # Display the parameters of the selected data point as two columns
      output$selectSample <- renderUI({
            selected <- selectSample()
            if (is.null(selected)) return(NULL)
          
            tagList(
                  h4("Selected Sample:"),
                  div(
                        fluidRow(
                              column(6, verbatimTextOutput("selectedSample1")),
                              column(6, verbatimTextOutput("selectedSample2"))
                        )
                  )
            )
      })
    
      output$selectedSample1 <- renderText({
            selected <- selectSample()
            if (is.null(selected)) return("No sample selected.")
          
            paste("Pregnancies:", selected$NumberPregnancies,
                  "\nGlucose:", selected$Glucose,
                  "\nBlood Pressure:", selected$BloodPressure,
                  "\nSkin Thickness:", selected$SkinThickness)
      })
    
      output$selectedSample2 <- renderText({
            selected <- selectSample()
            if (is.null(selected)) return(NULL)
          
            paste("Insulin:", selected$Insulin,
                  "\nBMI:", selected$BMI,
                  "\nPedigree Function:", selected$PedigreeFunction,
                  "\nAge:", selected$Age)
      })
    

      output$prediction <- renderUI({
            
            #print("Prediction Triggered")  # Debugging
          
            # Display a please select message for the user in the prediction panel as
            # long no data point was selected
            if (is.null(selectSample())) {
                  return(HTML("Please select a data point on the graphic displayed on the right side."))
            }
          
            selected <- selectSample()
          
            if (is.null(selected)) {
                  return(HTML("Please select a data point on the graphic displayed on the right side."))
            }
    
            # Select the training dataset
            train_data <- dataset[!rownames(dataset) %in% rownames(sampleData()), ]
      
            #print(nrow(train_data)) Debugging
    
            # Train a logistic regression model 
            model_logistic <- glm(diabetes ~ ., data = train_data, family = binomial)
            # Get the prediction of the selected sample
            prediction_logistic <- predict(model_logistic, selected, type = "response")
            # Get the probability
            probability_logistic <- round(prediction_logistic * 100, 2)
            class_logistic <- ifelse(prediction_logistic > 0.5, "Diabetes Positive", "Diabete Negative")
    
            # Similar steps for a decision tree model
            model_dtree <- rpart(diabetes ~ ., data = train_data, method = "class")
            prediction_tree_probs <- predict(model_dtree, selected, type = "prob")
            
            probability_tree <- round(prediction_tree_probs[,"pos"] * 100, 2)
            class_tree <- ifelse(prediction_tree_probs[,"pos"] > 0.5, "Diabetes Positive", "Diabetes Negative")
            
            # Save the true class of the sample
            true_answer <- ifelse(selected$diabetes == "pos", "Diabetes Positive", "Diabetes Negative")
            
            
            #print(class_logistic)            # Debugging
            #print(as.character(prediction_tree_probs)) # Debugging

            # Print out the result text that should be displayed in the iu
            result_text <- paste(
                  "<strong>Logistic Regression Prediction:</strong> <br/>", class_logistic, 
                  "<br/> (Probability: ", probability_logistic, "%)<br/><br/>",
                  "<strong>Decision Tree Prediction:</strong> <br/>", class_tree, 
                  "<br/> (Probability: ", probability_tree, "%)<br/><br/>",
                  "<strong>True Answer:</strong> <br/>", true_answer
            )
      
            HTML(result_text)
      })
      

}
