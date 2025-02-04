server <- function(input, output) {
  
  omega <- 110
  
  # input ui ----
  is_deffereal <- reactive({
    ifelse(input$advance_deferred_payment == "Deferred", 1, 0)
  })
  initial_age <- reactiveValues(max = omega, min = 1)
  observeEvent(input$age, {
    initial_age$min <- input$age
  })
  output$duration_coverage <- renderUI({
    sliderInput("duration_coverage",
                "Duration of coverage",
                min = input$age + is_deffereal(),
                max = initial_age$max + is_deffereal(),
                value = c(input$age + is_deffereal(), initial_age$max + is_deffereal()),
                step = 1
                )
  })
  
  output$number_premiums <- renderUI({
    sliderInput("number_premiums",
                "Number of premiums",
                min = 1,
                max = input$duration_coverage[1] - input$age + is_deffereal(),
                value = 1,
                step = 1
    )
  })
  
  output$guaranteed_rates_duration <- renderUI({
    sliderInput("guaranteed_rates_duration",
                "Duration of guaranteed rates",
                min = 0,
                max = input$duration_coverage[2] - input$duration_coverage[1],
                value = 0,
                step = 1
    )
  })

}