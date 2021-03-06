#-------------------------------------------------------------------
# Requirements
#-------------------------------------------------------------------
source('lib-justroomies.R')

#-------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(HTML('<h1 style="text-align:center;color:#987752"> <b>JUST</b> &#9878; ROOMIES</h1><br><br>')),
  theme = shinythemes::shinytheme("simplex"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("check_lang", HTML("<b>Show available languages</b>"), FALSE),
      conditionalPanel(condition = "input.check_lang",
        radioButtons("lang", "Choose a language:",
                     c("English" = "en",
                       "Deutsch" = "de",
                       "Español" = "es",
                       "Français" = "fr",
                       "Nederlands" = "nl"))
      ),
      HTML("<h5><b>Financials</b></h5>"),
      numericInput('total', 
                   'Total rent:',
                   value = 1500,
                   min = 1,
                   max = 30000),
      numericInput("min", 
                  "Bare minimum:", 
                  value = 150,
                  min = 1,
                  max = 3000),
      HTML("<br><h5><b>Income per roommate</b></h5>"),
      numericInput("roomies", 
                   "How many roomies?", 
                   value = 4,
                   min = 2,
                   max = 10),
      conditionalPanel(condition = "input.plotting_tabs == 'Bar plot'",
                       numericInput("incomeA", 
                                    "Aimé:", 
                                    value = 800,
                                    min = 1,
                                    max = 10000),
                       numericInput("incomeL", 
                                    "Lina:", 
                                    value = 600,
                                    min = 1,
                                    max = 10000),
                       conditionalPanel(condition = "input.roomies > 2",
                                        numericInput("incomeM",
                                                     "Maurice:", 
                                                     value = 400,
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
      ) # Close conditional tab is plotting tab
    ), # Close sidebar panel
    mainPanel(
      tabsetPanel(id = "plotting_tabs",
        tabPanel("Bar plot", 
                 HTML("<br>"),
                 uiOutput("explainBtn"),
                 HTML("<br><br><br><br>"),
                 plotOutput("bars")
                 ), #Close plotting tab 'Bar plot'
        tabPanel("Pie charts",
                 HTML("<br>"),
                 #uiOutput("explainBtn"),
                 HTML("<br><br><br><br>"),
                 plotOutput("mainPie")
          
        ), #Close plotting tab 'Pie charts'
        tabPanel("Table",
                 HTML("<br>"),
                 #uiOutput("explainBtn"),
                 HTML("<br><br><br><br>"),
                 tableOutput("myTable")
        ) # Close plotting tab table
      )
    )
  )
)


#-------------------------------------------------------------------
# Server
#-------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$explainBtn <- renderUI({ 
     
    actionButton("explanation", 
                 label = translateHowDoesItWork(input$lang), 
                 class = "btn-outline-info",
                 style = "position: absolute; right: 40px")
    })
  
  names <- c('Aimé', 'Lina', 'Maurice', 'Janice', 'Paola', 'Salomon', 'Norbert', 'Tomasz', 'Ursula', 'Françoise')
  colours <- c('#1ba1e2', '#60a917', '#a20025', '#c58608', '#0042c4', '#005800', '#6e4b05', '#6a00ff', '#b19e00', '#6d00a3')
  
  rcDistribution <- reactive({
    
    incomes <- c(input$incomeA, input$incomeL, input$incomeM, input$incomeJ, input$incomeP, input$incomeS, input$incomeN, input$incomeT, input$incomeU, input$incomeF)
    
    deliverJustDistribution(names[1:input$roomies],
                            incomes[1:input$roomies],
                            colours[1:input$roomies],
                            input$total, 
                            input$min,
                            input$lang)
    
  })
  
  
  output$bars <- renderPlot({rcDistribution()[[2]]})
  
  output$myTable <- renderTable({rcDistribution()[[1]]})
  
  output$mainPie <- renderPlot({
    
    rcDistribution()[[1]] %>%
      ggplot(aes(x = "", y = percentageOfRent, fill = roomie)) +
      coord_polar("y", start = 0) +
      geom_bar(stat = "identity", width = 1) +
      labs(x = "", y = "", title = deliverPieChartTitle(input$lang)) +
      geom_label(aes(label = percentageLabel), 
                 position = position_stack(vjust = 0.5), 
                 show.legend = FALSE,
                 label.size = 0.65) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.text = element_blank(),
            panel.grid  = element_blank(),
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 14)) +
      scale_fill_manual(values = rcDistribution()[[1]][['colour']]) 
      
      
  })
  
  
  
  observeEvent(input$explanation, {
    showModal(modalDialog(
      title = translateHowDoesItWork(input$lang),
      HTML(translateExplanation(input$lang)),
      easyClose = TRUE,
      size= 'l',
      footer = modalButton(translateOK(input$lang))
    ))
  })
}

#-------------------------------------------------------------------
# Run App
#-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
