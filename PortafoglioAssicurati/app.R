#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(lifecontingencies)
library(tibble)
library(ggplot2)

omega = 110

# Define UI for application that draws a histogram
ui <- fluidPage(

  
    # Application title
    titlePanel("Portafoglio di assicurati"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # style = "position:fixed;width:inherit;",
            # "Inputs",
            style = "height: 90vh; overflow-y: auto;",
            width = 3,
            numericInput("rate", "Valore delle rate", 12000, min = 0,step = 100),
            numericInput("tassoAleatorio", "Tasso aleatorio del fondo", .04, min = 0,step = .001),
            numericInput("tassoTecnico", "Tasso tecnico per il calcolo del premio", .03, min = 0,step = .001),
            numericInput("fondoIniziale", "Importo del fondo iniziale", 0, min = 0,step = 10000),
            numericInput("numeroAssicurati", "Numero di assicurati", 1000, min = 0,step = 100),
            sliderInput("eta",
                        "Eta degli assicurati",
                        min = 1,
                        max = omega,
                        value = 30),
            uiOutput("sliderDiffDurata"),
            uiOutput("sliderNumPremi"),
            uiOutput("sliderRateGarantiteDurata")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(tabPanel("Andamento Fondo", plotOutput("andamentoFondoPlot")),
                      tabPanel("Rendimento Fondo", plotOutput("rendimentoFondoPlot")))
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    output$andamentoFondoPlot = renderPlot({
      obj = gestionePortafoglio(eta = input$eta,
                                differimento = input$periodo[1] - input$eta,
                                anniCopertura = input$periodo[2] - input$eta,
                                numeroPremi = input$numPremi,
                                temporanea = T,
                                omega = omega,
                                rata = input$rate,
                                tassoAleatorio = input$tassoAleatorio,
                                tassoTecnico = input$tassoTecnico,
                                numeroAssicurati = input$numeroAssicurati,
                                fondoInizio = input$fondoIniziale,
                                rateGarantiteDurata = input$rateGarantite)
      stampaGrafici(obj)[[1]]
      })
    output$rendimentoFondoPlot = renderPlot({
      obj = gestionePortafoglio(eta = input$eta,
                                differimento = input$periodo[1] - input$eta,
                                anniCopertura = input$periodo[2] - input$eta,
                                numeroPremi = input$numPremi,
                                temporanea = T,
                                omega = omega,
                                rata = input$rate,
                                tassoAleatorio = input$tassoAleatorio,
                                tassoTecnico = input$tassoTecnico,
                                numeroAssicurati = input$numeroAssicurati, 
                                fondoInizio = input$fondoIniziale)
      stampaGrafici(obj)[[2]]
      })
    
    
    ###INPUT###
    # SlideBar: differimento e durata
    etaIniziale <- reactiveValues(max = omega, value = 1)
    output$sliderDiffDurata <- renderUI({
    sliderInput("periodo",
                  "Eta degli assicurati",
                  min = etaIniziale$value,
                  max = omega,
                  value = c(35,65)
                  )
    })
    observeEvent(input$eta, {
      etaIniziale$value <- input$eta + 1
    })
    
    # SlideBar: numero di premi
    numPremiValue = reactiveValues(value = 1)
    premiMax <- reactiveValues(max = omega, value = 4)
    output$sliderNumPremi <- renderUI({
      sliderInput("numPremi",
                  "Numero di premi",
                  min = 1,
                  max = premiMax$value,
                  value = numPremiValue$value,
                  step = 1
      )
    })
    observeEvent(input$eta, {
      premiMax$value <- input$periodo[1] - input$eta
      # numPremiValue$value = input$numPremi
    })
    observeEvent(input$periodo, {
      premiMax$value <- input$periodo[1] - input$eta
      # numPremiValue$value = input$numPremi
    })
    
    # slider rate garantite
    rateGarantiteMax <- reactiveValues(max = omega, value = 1)
    output$sliderRateGarantiteDurata <- renderUI({
      sliderInput("rateGarantite",
                  "Numero di rate garantite",
                  min = 0,
                  max = rateGarantiteMax$value,
                  value = 0,
                  step = 1
      )
    })
    observeEvent(input$periodo, {
      rateGarantiteMax$value <- input$periodo[2] - input$periodo[1]
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
