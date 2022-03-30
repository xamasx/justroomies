

#-------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Cost distribution"),
  theme = shinythemes::shinytheme("simplex"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("total", 
                  "Total rent (MUCHes):", 
                  value = 1450,
                  min = 1,
                  max = 3000,
                  step = 10),
      sliderInput("min", 
                  "Bare minimum (MUCHes):", 
                  value = 200,
                  min = 1,
                  max = 3000,
                  step = 10),
      sliderInput("incomeA", 
                  "Income Aime (MUCHes):", 
                  value = 800,
                  min = 1,
                  max = 3000,
                  step = 10),
      sliderInput("incomeL", 
                  "Income Lina (MUCHes):", 
                  value = 800,
                  min = 1,
                  max = 3000,
                  step = 10),
      sliderInput("incomeM", 
                  "Income Maurice (MUCHes):", 
                  value = 800,
                  min = 1,
                  max = 3000,
                  step = 10),
      sliderInput("incomeJ", 
                  "Income Janice (MUCHes):", 
                  value = 800,
                  min = 1,
                  max = 3000,
                  step = 10)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", plotOutput("bars"))
      )
    )
  )
)

#-------------------------------------------------------------------
# Server
#-------------------------------------------------------------------
server <- function(input, output, session) {
  
  rcDistribution <- reactive({
    
    deliverJustDistribution(c('Aime', 'Lina', 'Maurice', 'Janice'),
                            c(input$incomeA, input$incomeL, input$incomeM, input$incomeJ),
                            input$total, input$min)
    
  })
  
  
  output$bars <- renderPlot({rcDistribution()[[2]]})
}

#-------------------------------------------------------------------
# Run App
#-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
