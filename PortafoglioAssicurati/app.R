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
            numericInput("tassoTecnico", "Tasso tecnico per il calcolo del premio", .03, min = 0,step = .001),
            numericInput("rendimentoFondoAnnuo", "Rendimento annuo del fondo", .03, min = 0,step = .001),
            checkboxInput("tassoAleatorio", "Rendimento aleatorio del fondo", value = TRUE),
            numericInput("fondoIniziale", "Importo del fondo iniziale", 0, min = 0,step = 10000),
            numericInput("numeroAssicurati", "Numero di assicurati", 1000, min = 0,step = 100),
            sliderInput("eta",
                        "Eta degli assicurati",
                        min = 1,
                        max = omega,
                        value = 30),
            uiOutput("sliderDiffDurata"),
            uiOutput("sliderNumPremi"),
            uiOutput("sliderRateGarantiteDurata"),
            # si potrebbe anche inserire tavole di diversa nazionalità
            selectInput("tavolaTecnica", "Tavola tecnica", names(demoIta), selected = names(demoIta)[6]),
            selectInput("tavolaPeriodo", "Tavola di periodo", names(demoIta), selected = names(demoIta)[6])
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(tabPanel("Andamento Fondo", plotOutput("andamentoFondoPlot")),
                      tabPanel("Rendimento Fondo", plotOutput("rendimentoFondoPlot")),
                      tabPanel("Decessi", plotOutput("decessi")),
                      tabPanel("Monte Carlo", plotOutput("monteCarloPlot"))
                      )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
  library(lifecontingencies)
  
  # configurazione variabili iniziali
  numPremiIniziale = 1
   
  callPortafoglio = reactive(
  {
    return(gestionePortafoglio(eta = input$eta,
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
                               rateGarantiteDurata = input$rateGarantite,
                               rendimentoFondoAnnuo = input$rendimentoFondoAnnuo,
                               tavolaPeriodo = demoIta[,names(demoIta) == input$tavolaPeriodo], 
                               tavolaMortalita = demoIta[,names(demoIta) == input$tavolaTecnica]))
    numPremiIniziale = numPremiValue$value
  })
  
    output$monteCarloPlot = renderPlot({
      x = c()
      for(i in 1:1E3)
      {
        # v = callPortafoglio()$andamentoFondo;
        v = gestionePortafoglio(eta = input$eta,
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
                            rateGarantiteDurata = input$rateGarantite,
                            rendimentoFondoAnnuo = input$rendimentoFondoAnnuo,
                            tavolaPeriodo = demoIta[,names(demoIta) == input$tavolaPeriodo], 
                            tavolaMortalita = demoIta[,names(demoIta) == input$tavolaTecnica])$andamentoFondo
        x = c(x,v[length(v)])
      }
      x %>% 
        tibble %>% 
        ggplot(aes(x, y = ..density..)) +
        geom_histogram(color = "orchid",
                       fill = "orchid",
                       alpha = 0.5,
                       bins = 50) +
        geom_density(color = "paleturquoise",
                     fill = "paleturquoise",
                     alpha = 0.3, # densità del colore 
                     kernel = "gaussian",
                     adjust = 1) +
        geom_vline(xintercept = 0,
        color = "orchid4") +
        annotate("text",
                 x = 2E5,
                 y = max(density(x)$y),
                 label = sum(x>0)/length(x),
                 color = "orchid4")
    })
  
    output$andamentoFondoPlot = renderPlot({
      obj = callPortafoglio()
      stampaGrafici(obj)[[1]]
      })
    
    output$rendimentoFondoPlot = renderPlot({
      obj = callPortafoglio()
      stampaGrafici(obj)[[2]]
      })
    
    output$decessi = renderPlot({
      obj = callPortafoglio()
      stampaGrafici(obj)[[3]]
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
    numPremiValue = reactiveValues(value = numPremiIniziale)
    premiMax <- reactiveValues(max = omega, value = 4)
    output$sliderNumPremi <- renderUI({
      sliderInput("numPremi",
                  "Numero di premi",
                  min = 1,
                  max = premiMax$value,
                  value = numPremiIniziale,
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
