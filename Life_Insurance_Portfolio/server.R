server <- function(input, output) {
  
  # input ui ----
  is_deffereal <- reactive({
    ifelse(input$payment == "Deferred", 1, 0)
  })
  
  omega <- reactive({
    max(lifecontingencies::demoIta[,"X"]) 
    # for the future, when it will be possible to select different tables
    # max(
    #   length(lifecontingencies::demoIta[,names(demoIta) == input$technical_table]),
    #   length(lifecontingencies::demoIta[,names(demoIta) == input$simulation_table])
    # ) -1
  })
  
  # to keep inalterated the values of number_premiums if they are valid 
  initial_input <- reactiveValues(
    number_premiums_value = 20,
    guaranteed_rates_duration_value = 5,
    duration_annuities_values = c(60,85)
    )
  
  output$duration_annuities <- renderUI({
    sliderInput("duration_annuities",
                "Duration of annuities",
                min = input$age + is_deffereal(),
                max = omega() + is_deffereal(),
                value = initial_input$duration_annuities_values,
                step = 1
                )
  })
  
  output$number_premiums <- renderUI({
    sliderInput("number_premiums",
                "Number of premiums",
                min = 1,
                max = input$duration_annuities[1] - input$age + is_deffereal(),
                value = initial_input$number_premiums_value,
                step = 1
    )
  })
  

  output$guaranteed_rates_duration <- renderUI({
    sliderInput("guaranteed_rates_duration",
                "Duration of guaranteed rates",
                min = 0,
                max = input$duration_annuities[2] - input$duration_annuities[1],
                value = initial_input$guaranteed_rates_duration_value,
                step = 1
    )
  })
  
  observeEvent(input$age, {
    if(!is.null(input$duration_annuities))
    {
      initial_input$duration_annuities_values[1] <- 
        max(input$age + is_deffereal(), input$duration_annuities[1])
      initial_input$duration_annuities_values[2] <- 
        min(omega() + is_deffereal(), input$duration_annuities[2])
    }
    })

  observeEvent(input$duration_annuities, {
    if(!is.null(input$number_premium))
    {
      initial_input$number_premiums_value <- 
        min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
    }
    
    if(!is.null(input$guaranteed_rates_duration))
    {
      initial_input$guaranteed_rates_duration_value <- 
        min(input$guaranteed_rates_duration, input$duration_annuities[2] - input$duration_annuities[1])
    }
  })
  
  observeEvent(input$payment, {
    if(!is.null(input$duration_annuities) && !is.null(input$number_premiums))
    {
      initial_input$number_premiums_value <- 
        min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
    }
    
    if(!is.null(input$duration_annuities))
    {
      initial_input$duration_annuities_values[1] <- 
        max(input$age + is_deffereal(), input$duration_annuities[1])
      initial_input$duration_annuities_values[2] <- 
        min(omega() + is_deffereal(), input$duration_annuities[2])
    }
    
  })
  
  # render data 
  data_fund <- reactive({
    # message(
    #   paste0("data_fund \n",
    #          "number_insured: ", input$number_insured, "\n",
    #          "age: ", input$age, "\n",
    #          "annuity: ", input$annuity, "\n",
    #          "initial_fund: ", input$initial_fund, "\n",
    #          "number_premiums: ", input$number_premiums, "\n",
    #          "omega: ", omega(), "\n",
    #          "deffered: ", input$duration_annuities[1] - input$age, "\n",
    #          "payment: ", input$payment, "\n",
    #          "coverage_years: ", input$duration_annuities[2] - input$age, "\n",
    #          "guaranteed_rates_duration: ", input$guaranteed_rates_duration, "\n",
    #          "fund_return_rate: ", input$interest_rate, "\n",
    #          "technical_rate: ", input$technical_rate, "\n",
    #          "aleatory_rate: ", input$aleatory_rate, "\n",
    #          "mortality_table: ", input$technical_table, "\n",
    #          "simulation_table: ", input$simulation_table, "\n",
    #          "aleatory_mortality: ", input$aleatory_mortality, "\n"
    #         )
    # )
    fund(
      number_insured = input$number_insured,
      age = input$age,
      annuity = input$annuity,
      initial_fund = input$initial_fund,
      number_premiums = input$number_premiums,
      omega = omega(),
      deffered = input$duration_annuities[1] - input$age,
      payment = input$payment,
      coverage_years = input$duration_annuities[2] - input$age,
      guaranteed_rates_duration = input$guaranteed_rates_duration,
      fund_return_rate = input$interest_rate,
      technical_rate = input$technical_rate,
      aleatory_rate = input$aleatory_rate,
      mortality_table = lifecontingencies::demoIta[,names(demoIta) == input$technical_table],
      simulation_table = lifecontingencies::demoIta[,names(demoIta) == input$simulation_table],
      aleatory_mortality = input$aleatory_mortality
    )
  })
  
  # FUND PERFORMANCE tabset ----
  output$fund_performance_plot <- renderPlot({
    data_fund() |> 
      plot_fund_performance()
  })
  
  output$fund_spin_plot <- renderPlot({
    data_fund() |> 
      plot_fund_spin()
  })
  
  output$real_fund_table <- renderDataTable({
    table_real_fund(data_fund())
    })
  
  # FUND THEORETICAL tabset ----
  output$theoretical_fund_plot <- renderPlot({
    data_fund() |> 
      plot_fund_theoretical()
  })
  
  output$theoretical_fund_table <- renderDataTable({
    table_theoretical_fund(data_fund())
  })
  
  # DEATHS AND FINANCIAL RATES tabset ----
  
  output$deaths_plot <- renderPlot({
    plot_deaths(data_fund())
  })
  
  

}