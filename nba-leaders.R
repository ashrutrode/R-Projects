# Install and load required packages if not already installed
if (!require(shiny)) install.packages("shiny")
library(shinythemes)

# Specify the path to CSV file
csv_file_path <- "NBA Leaders.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file_path)

# Define UI
ui <- fluidPage(
  
  # Theme
  theme = shinytheme("slate"),
  
  # Title
  titlePanel("NBA Leaders"),
  
  # Layout
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable", choices = names(data), selected = "Points"),
      selectInput("order", "Sort Order", choices = c("Descending", "Ascending"), selected = "Descending")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to sort and display the table
  sorted_data <- reactive({
    
    # Either increasing or decreasing order
    if (input$order == "Descending") {
      choice_multiplier <- -1
      alpha_descreasing = TRUE
    } else if (input$order == "Ascending") {
      choice_multiplier <- 1
      alpha_descreasing = FALSE
    }
    
    # Sorting data
    if (input$variable == "Player") { # Sorting by player name
      ordered_data <- data[order(data$Player, decreasing = alpha_descreasing), ]
    } else {  # Sorting anything else (a number)
      ordered_data <- data[order(choice_multiplier*data[[input$variable]]), ]
    }
    
  })
  
  # Render the table
  output$table <- renderTable({
    sorted_data()
  })
  
}

# Run the application
shinyApp(ui, server)
