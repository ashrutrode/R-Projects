# Load R packages
library(shiny)
library(shinythemes)

# install required library
library(tidyverse)

#convert to new format
titanic <- as_tibble(Titanic)

# Define UI
ui <- fluidPage(theme = shinytheme("spacelab"),
                
    # Page header
    headerPanel('Survival Rates on the Titanic'),
    
    # Input values
    sidebarPanel(
      HTML("<h4>Demographics</h4>"),
      checkboxGroupInput("class", "Class:", c("1st" = "1st", "2nd" = "2nd", "3rd" = "3rd", "Crew" = "Crew"), c("1st", "2nd", "3rd", "Crew")),
      checkboxGroupInput("sex", "Sex:", c("Male" = "Male", "Female" = "Female"), c("Male", "Female")),
      checkboxGroupInput("age", "Age:", c("Child" = "Child", "Adult" = "Adult"), c("Child", "Adult")),
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
    ),
    
    mainPanel(
      HTML("<h4>Survival Statistics</h4>"),
      verbatimTextOutput('contents')
      
    )
                
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  # when submit button is clicked
  observeEvent(input$submitbutton, {
    
   # if user leaves any blank, then show an error
   if (length(input$class) == 0 || length(input$sex) == 0 || length(input$age) == 0) {
     output$contents <- renderPrint({
       paste("Please enter at least one selection for each category.")
     })
   }
    
   # if at least one checkbox is selected in each category
   else {
     
     # convert into vectors
     class_vector <- strsplit(input$class, " ")
     sex_vector <- strsplit(input$sex, " ")
     age_vector <- strsplit(input$age, " ")
     
     # sum only based on conditions
     no_survived <- sum(
       subset(
         titanic, 
         Class %in% class_vector & 
           Sex %in% sex_vector & 
           Age %in% age_vector & 
           Survived == "No")$n
     )
     yes_survived <- sum(
       subset(
         titanic, 
         Class %in% class_vector & 
           Sex %in% sex_vector & 
           Age %in% age_vector & 
           Survived == "Yes")$n
     )
     total_passengers = yes_survived+no_survived
     
     # probability of selection surviving
     survival_percent <- round(100*yes_survived/total_passengers, digits = 2)
     
     # rendering the solution
     output$contents <- renderPrint({
       paste("Total passengers:", total_passengers, "; Num of Survivors: ", yes_survived, "; Num of Deaths: ", no_survived, "; Survival Rate: ", survival_percent, "%")
     })
     
   }
  
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
