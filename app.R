#-------------------------------------------------------------------
# Requirements
#-------------------------------------------------------------------
source('lib-justroomies.R')

#-------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(HTML('<h1 style="text-align:center;color:#987752"> <b>JUST</b> &#9878; ROOMIES</h1>')),
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
      HTML("<br><h5><b>Income per roomie</b></h5>"),
      numericInput("roomies", 
                   "How many roomies?", 
                   value = 4,
                   min = 2,
                   max = 10),
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
                 actionButton("show", "How does it work?", 
                              class = "btn-outline-info",
                              style = "position: absolute; right: 40px"),
                 HTML("<br><br><br><br>"),
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
      title = "How does it work?",
      HTML("<p> When I lived in Berlin I shared my flat with three other people. 
      We all had different incomes. Our rooms had all diffeent surfaces. 
      One of my roomies said, 'Well, if you want to be fair, you'd need to split 
      the costs according to the surface each one of us lives in'. And we did just that. 
      But, deep down, it did not feel right. And when my girlfirend at the time
      started struggling, I was convinced there had to be another way.<br>
      
      So, for those who currently live in a shared flat and would like to split 
      the costs according to what each of you earn, here is a simple calculator
      that can help you do just that. <br><br>
      
      Under <b>Financials</b> set the total amount that you and your roomies would like
      to split in a Monetary Unit of your CHoice (MUCH). It could be USD, EUR or anything 
      else. Under <b>Income per roomie</b> choose the number of people who share the flat. <br><br>
      
      A number of hypothetical names will appear. Each one of your roomies should pick a name
      and type in their corresponding monthly income in the box - if one of your roomies 
      is a free-lance, this will obviously vary month to month. <br>
      Having all entered how much MUCHes you earned this moth, the plot will 
      automatically adjust and you will be able to see how much MUCHes each one of 
      you ought to pay this month. <br><br>
      
      In cases where there is a stark imbalance among roomies, you may want to consider
      discussing a bare minimum that you all agree with. This way, you ensure that any
      given month where, for example, Maurice happened to struggle does not take a 
      stark toll on high-earner Lina. </p>"),
      easyClose = TRUE,
      size= 'l',
      footer = modalButton("Ok, thanks!")
    ))
  })
}

#-------------------------------------------------------------------
# Run App
#-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
