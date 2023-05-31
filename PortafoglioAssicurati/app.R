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
            sliderInput("eta",
                        "Eta degli assicurati",
                        min = 1,
                        max = omega,
                        value = 30),
            uiOutput("sliderDiffDurata"),
            uiOutput("sliderNumPremi")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("andamentoFondoPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
    output$andamentoFondoPlot = renderPlot({
      obj = gestionePortafoglio(eta = input$eta,
                                differimento = input$periodo[1] - input$eta,
                                anniCopertura = input$periodo[2] - input$eta,
                                numeroPremi = input$numPremi,
                                temporanea = T)
      stampaGrafici(obj)[[1]]
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
      etaIniziale$value <- input$eta
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
      numPremiValue = input$numPremi
    })
    observeEvent(input$periodo, {
      premiMax$value <- input$periodo[1] - input$eta
      numPremiValue = input$numPremi
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
