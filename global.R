
library(shiny)
library(mlbench)
library(ggplot2)
library(caret)
library(rpart)
library(plotly)

data("PimaIndiansDiabetes")
dataset <- PimaIndiansDiabetes
dataset <- na.omit(dataset)
#BreastCancer <- BreastCancer[,-1]  # Removing the 'Id' column
dataset[,-9] <- lapply(dataset[,-9], function(x) as.numeric(as.character(x)))
colnames(dataset) <- c("NumberPregnancies", "Glucose", "BloodPressure", 
                       "SkinThickness", "Insulin", "BMI", "PedigreeFunction", 
                       "Age", "diabetes")
dataset$diabetes <- factor(dataset$diabetes, levels = c("neg", "pos"))



documentationModal <- function() {
      modalDialog(
            title = "Documentation",
            p("This application predicts the diabetes status of a patient based on various health metrics. To use the app:"),
            tags$ul(
                  tags$li("Select 2 parameters for the X and Y axes from the dropdown menus. The available parameters are described in the Show Data Information section.",
                          "By default, the Age parameter is on the X-axis, and the Glucose parameter on the Y-axis."
                  ),
                  tags$li("Adjust the number of points displayed on the graphic on the right using the slider. Each point represents one patient."),
                  tags$li("Click on any point in the scatter plot to chose a patient. The selected point will appear as red, and the detailed health metrics of this particular patient will be displayed below the graphic. "),
                  tags$li("Predictions are shown using both Logistic Regression and Decision Tree models.")
            ),
            
            actionButton("show_data_info", "Show Data Information"),
            
            footer = modalButton("Close")
      )
}