#-------------------------------------------------------------------
# Requirements
#-------------------------------------------------------------------
source('lib-justroomies.R')

#-------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(HTML('<h1 style="text-align:right;color:#987752"> <b>JUST</b> &#9878; ROOMIES</h1>')),
  theme = shinythemes::shinytheme("simplex"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h5><b>Financials</b></h5>"),
      numericInput('total', 
                   'Total rent:',
                   value = 1450,
                   min = 1,
                   max = 3000),
      numericInput("min", 
                  "Bare minimum:", 
                  value = 200,
                  min = 1,
                  max = 3000),
      numericInput("roomies", 
                   "How many roomies?", 
                   value = 4,
                   min = 2,
                   max = 10),
      HTML("<br><h5><b>Income per roomie</b></h5>"),
      numericInput("incomeA", 
                  "Aimé:", 
                  value = 800,
                  min = 1,
                  max = 10000),
      numericInput("incomeL", 
                  "Lina:", 
                  value = 800,
                  min = 1,
                  max = 10000),
      conditionalPanel(condition = "input.roomies > 2",
                       numericInput("incomeM",
                                    "Maurice:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
        ),
      conditionalPanel(condition = "input.roomies > 3",
                       numericInput("incomeJ", 
                                    "Janice:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 4",
                       numericInput("incomeP", 
                                    "Paola:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 5",
                       numericInput("incomeS", 
                                    "Solomon:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 6",
                       numericInput("incomeN", 
                                    "Norbert:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 7",
                       numericInput("incomeT", 
                                    "Tomasz:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 8",
                       numericInput("incomeU", 
                                    "Ursula:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      ),
      conditionalPanel(condition = "input.roomies > 9",
                       numericInput("incomeF", 
                                    "Françoise:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar plot", 
                 HTML("<br>"),
                 actionButton("show", "What is this?", class = "btn-outline-info"),
                 HTML("<br><br>"),
                 plotOutput("bars"))
      )
    )
  )
)


#-------------------------------------------------------------------
# Server
#-------------------------------------------------------------------
server <- function(input, output, session) {
  
  rcDistribution <- reactive({
    
    names <- c('Aimé', 'Lina', 'Maurice', 'Janice', 'Paola', 'Salomon', 'Norbert', 'Tomasz', 'Ursula', 'Françoise')
    incomes <- c(input$incomeA, input$incomeL, input$incomeM, input$incomeJ, input$incomeP, input$incomeS, input$incomeN, input$incomeT, input$incomeU, input$incomeF)
    colours <- c('#1ba1e2', '#60a917', '#a20025', '#f2b336', '#0042c4', '#005800', '#6e4b05', '#6a00ff', '#b19e00', '#6d00a3')
    
    deliverJustDistribution(names[1:input$roomies],
                            incomes[1:input$roomies],
                            colours[1:input$roomies],
                            input$total, input$min)
    
  })
  
  
  output$bars <- renderPlot({rcDistribution()[[2]]})
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Important message",
      "This is an important message!",
      easyClose = TRUE
    ))
  })
}

#-------------------------------------------------------------------
# Run App
#-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
