#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggplot2)

studentData = read.csv("Data/Student_GPA_Data.csv")

lr = lm(GPA ~ Age+Absences+Tutoring+ParentalSupport+StudyTimeWeekly, data=studentData)


# Run the application 
# Define UI ----
ui <- page_sidebar(
  title = "GPA Prediction using Linear Regression Modeling",
  sidebar = sidebar(
    helpText(
      "Enter independent variables: Absences, StudyTimeWeekly, Tutoring, ParentalSupport, Age."
    ),
    sliderInput(
      "Absences",
      "Absences for the Year",
      min = 0,
      max = 30,
      value = 5
    ),
    sliderInput(
      "StudyTimeWeekly",
      "Study Time Weekly",
      min = 0,
      max = 20,
      value = 10
    ),
    input_switch("Tutoring", "Tutoring"),
    sliderInput(
      "ParentalSupport",
      "Parental Support",
      min = 0,
      max = 4,
      value = 2
    ),
    sliderInput(
      "Age",
      "Age",
      min = 15,
      max = 19,
      value = 17
    ),
    selectInput(
      "variable_to_plot",
      "Select a factor for visualization",
      choices = c("Absences", "StudyTimeWeekly", "Tutoring", "ParentalSupport", "Age"),
      selected = "StudyTimeWeekly"
    ),
    tags$style(HTML("
      #predict_button {
        background-color: #007bff;
        color: white;
        border: none;
        padding: 10px 15px;
        font-size: 14px;
        cursor: pointer;
        border-radius: 5px;
      }
      #predict_button:hover {
        background-color: #0056b3;
      }
    ")),
    actionButton("predict_button", "Predict GPA")
  ),
  navset_tab( 
    nav_panel("Prediction", 
              h3("Modelling based on following inputs: "),
              verbatimTextOutput("Confirmation"),
              h3("Predicted GPA:"),
              verbatimTextOutput("prediction"),
              h3("Visualize impact of a specific variable"),
              plotOutput("gpaPlot")
    ), 
    nav_panel("Explaination", 
              includeHTML("CaseStudy3_GPA.html")
    ), 
  ), 
  id = "tab" 
)

# Define server logic ----
server <- function(input, output) {
  
  observeEvent(input$predict_button, {
    output$Confirmation <- renderText({
      paste(
        " Absences:", input$Absences, "\n",
        "Weekly Study Time (hours):", input$StudyTimeWeekly, "\n",
        "Tutoring:", ifelse(input$Tutoring == 1, "Yes", "No"), "\n",
        "Parental Support:", input$ParentalSupport, "\n",
        "Age:", input$Age, "\n"
        
      )
    })
    
    output$prediction <- renderText({
      
      new_data <- data.frame(
        Age = as.numeric(input$Age),
        Absences = as.numeric(input$Absences),
        Tutoring = as.numeric(input$Tutoring),
        ParentalSupport = as.numeric(input$ParentalSupport),
        StudyTimeWeekly = as.numeric(input$StudyTimeWeekly)
      )
 
      pred <- predict(lr, new_data)
      
    })
    
    # Generate plot of predictions for the selected variable
    output$gpaPlot <- renderPlot({
      
      # Generate plot for the selected variable
      output$gpaPlot <- renderPlot({
        # Define a sequence of values for the selected variable
        selected_var <- input$variable_to_plot
        var_seq <- switch(
          selected_var,
          "Absences" = seq(0, 30, by = 1),
          "StudyTimeWeekly" = seq(0, 20, by = 1),
          "Tutoring" = c(0, 1),
          "ParentalSupport" = seq(0, 4, by = 0.5),
          "Age" = seq(15, 19, by = 0.5)
        )
        
        # Create a data frame with constant values for other variables
        plot_data <- data.frame(
          Age = as.numeric(input$Age),
          Absences = as.numeric(input$Absences),
          Tutoring = as.numeric(input$Tutoring),
          ParentalSupport = as.numeric(input$ParentalSupport),
          StudyTimeWeekly = as.numeric(input$StudyTimeWeekly)
        )
        
        # Update the selected variable in the data frame
        plot_data <- plot_data[rep(1, length(var_seq)), ]
        plot_data[[selected_var]] <- var_seq
        
        # Predict GPA for each value of the selected variable
        plot_data$PredictedGPA <- predict(lr, plot_data)
        
        # Plot the predictions
        ggplot(plot_data, aes_string(x = selected_var, y = "PredictedGPA")) +
          geom_line(color = "blue", size = 1) +
          geom_point(color = "blue", size = 2) +
          labs(
            title = paste("Predicted GPA vs", selected_var),
            x = selected_var,
            y = "Predicted GPA"
          ) +
          theme_minimal()
      })
      
 
    })
    
  })
  

  

}

# Run the app ----
shinyApp(ui = ui, server = server)