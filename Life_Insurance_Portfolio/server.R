server <- function(input, output) {
  
  # input ui ----
  is_deffereal <- reactive({
    ifelse(input$advance_deferred_payment == "Deferred", 1, 0)
  })
  omega <- reactive({
    max(lifecontingencies::demoIta[,"X"]) 
    # for the future, when it will be possible to select different tables
    # max(
    #   length(lifecontingencies::demoIta[,names(demoIta) == input$technical_table]),
    #   length(lifecontingencies::demoIta[,names(demoIta) == input$simulation_table])
    # ) -1
  })
  # initial_age <- reactiveValues(max = omega, min = 1)
  # observeEvent(input$age, {
  #   initial_age$min <- input$age
  # })
  output$duration_annuities <- renderUI({
    sliderInput("duration_annuities",
                "Duration of annuities",
                min = input$age + is_deffereal(),
                max = omega() + is_deffereal(),
                value = c(input$age + is_deffereal(), omega() + is_deffereal()),
                step = 1
                )
  })
  
  output$number_premiums <- renderUI({
    sliderInput("number_premiums",
                "Number of premiums",
                min = 1,
                max = input$duration_annuities[1] - input$age + is_deffereal(),
                value = 1,
                step = 1
    )
  })
  
  output$guaranteed_rates_duration <- renderUI({
    sliderInput("guaranteed_rates_duration",
                "Duration of guaranteed rates",
                min = 0,
                max = input$duration_annuities[2] - input$duration_annuities[1],
                value = 0,
                step = 1
    )
  })
  
  # render data 
  data_fund <- reactive(
    fund(
      number_insured = input$number_insured,
      age = input$age,
      annuity = input$annuity,
      initial_fund = input$initial_fund,
      number_premiums = input$number_premiums,
      omega = omega(),
      deffered = input$duration_annuities[1] - input$age,
      advance_deferred_payment = input$advance_deferred_payment,
      coverage_years = input$duration_annuities[2] - input$age,
      guaranteed_rates_duration = input$guaranteed_rates_duration,
      fund_return_rate = input$interest_rate,
      technical_rate = input$technical_rate,
      aleatory_rate = input$aleatory_rate,
      mortality_table = lifecontingencies::demoIta[,names(demoIta) == input$technical_table],
      simulation_table = lifecontingencies::demoIta[,names(demoIta) == input$simulation_table],
      aleatory_mortality = input$aleatory_mortality
    )
  )
  
  # FUND PERFORMANCE tabset ----
  output$fund_performance_plot <- renderPlot({
    data_fund() |> 
      ggplot(aes(age, fund)) +
      geom_line() +
      geom_point() +
      labs(title = "Fund value over time",
           x = "Age",
           y = "Fund value") +
      theme_minimal()
  })
  
  output$fund_performance_table <- renderDataTable({
    data_fund() |> 
      mutate(
        across(
          c(fund, fund_return, fund_premium, premium_value, fund_annuity),
          # \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
          round
        ),
        across(
          financial_rate,
          \(x) scales::percent(x, accuracy = .01)
        ),
      )
    
  })
  

}