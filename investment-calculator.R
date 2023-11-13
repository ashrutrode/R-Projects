# Install and load required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(plotly)) install.packages("plotly")

# Define UI
ui <- fluidPage(
  titlePanel("Investment Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("principal", "Principal Amount", value = 1000),
      numericInput("interest_rate", "Interest Rate as Percent:", value = 10),
      numericInput("years", "Number of Years:", value = 10)
    ),
    
    mainPanel(
      plotlyOutput("linePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to generate the line equation
  lineEquation <- reactive({
    
    # figuring out x end range
    if (input$years%%5 == 0) {x_end_range <- input$years}
    else {x_end_range <- input$years+5-input$years%%5}
    
    # line characteristics
    principal <- input$principal
    rate <- input$interest_rate
    years <- seq(0, x_end_range, length.out = input$years+1)
    amount <- principal * (1 + rate/100)^years
    
    # graph line
    data.frame(years, amount)
    
  })
  
  # Render the interactive line plot
  output$linePlot <- renderPlotly({
    plot_ly(data = lineEquation(), x = ~years, y = ~amount, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Growth Over Time", xaxis = list(title = "Years"), yaxis = list(title = "Amount ($)"))
  })
  
}

# Run the application
shinyApp(ui, server)
