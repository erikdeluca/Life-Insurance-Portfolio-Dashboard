server <- function(input, output, session) {
  
  # to keep inalterated the values of number_premiums if they are valid 
  initial_input <- reactiveValues(
    number_premiums_value = 20,
    guaranteed_rates_duration_value = 5,
    duration_annuities_values = c(60,85),
    technical_year_table_value = 2021,
    simulation_year_table_value = 2021,
    technical_table_soa_value = "sim91",
    simulation_table_soa_value = "sim91",
    max_age = 100
    )

  output$technical_year_table <- renderUI({
    selectInput("technical_year_table",
                "Technical table",
                choices = mapping_life_tables[[input$technical_country_HLD]],
                selected = initial_input$technical_year_table_value,
                multiple = FALSE
    )
  })
  
  output$simulation_year_table <- renderUI({
    selectInput("simulation_year_table",
                "Simulation table",
                choices = mapping_life_tables[[input$simulation_country_HLD]],
                selected = initial_input$simulation_year_table_value,
                multiple = FALSE
    )
  })
  
  output$technical_table_soa <- renderUI({
    selectInput("technical_table_soa",
                "Technical table",
                choices = mapping_soa_tables[[input$technical_country_soa]],
                selected = initial_input$technical_table_soa_value,
                multiple = FALSE
    )
  })
  
  output$simulation_table_soa <- renderUI({
    selectInput("simulation_table_soa",
                "Simulation table",
                choices = mapping_soa_tables[[input$simulation_country_soa]],
                selected = initial_input$simulation_table_soa_value,
                multiple = FALSE
    )
  })

  life_table_technical <- reactive({
    selected_tab <- ifelse(!is.null(input$active_tab), input$active_tab, "HLD")
    
    if (selected_tab == "HLD") {
      message(paste("technical year", input$technical_year_table))
      current_life_table <- life_tables |>
        filter(
          country == input$technical_country_HLD,
          year == ifelse(!is.null(input$technical_year_table), input$technical_year_table, initial_input$technical_year_table_value),
          sex == input$technical_sex_table
        ) |> pull(l_x)
    } else if (selected_tab == "SOA") {
      message("input$technical_table_soa ", input$technical_table_soa)
      message("input$technical_country_soa ", input$technical_country_soa)
      selected_soa_table <- initial_input$technical_table_soa_value
      current_life_table <- soa_tables[[input$technical_country_soa]] |>
        filter(selected_soa_table != 0) |> 
        pull(selected_soa_table)
    }else{
      warning("selected_tab is not HLD or SOA: ", selected_tab)
    }
    
    return(current_life_table)
  })
  
  life_table_simulation <- reactive({
    selected_tab <- ifelse(!is.null(input$active_tab), input$active_tab, "HLD")
    
    if (selected_tab == "HLD") {
      current_life_table <- life_tables |>
        filter(
          country == input$simulation_country_HLD,
          year == ifelse(!is.null(input$simulation_year_table), input$simulation_year_table, initial_input$simulation_year_table_value),
          sex == input$simulation_sex_table
        ) |> pull(l_x)
    } else if (selected_tab == "SOA") {
      selected_soa_table <- ifelse(!is.null(input$simulation_table_soa), input$simulation_table_soa, initial_input$simulation_table_soa_value)
      current_life_table <- soa_tables[[input$simulation_country_soa]] |> 
        filter(selected_soa_table != 0) |> 
        pull(selected_soa_table)
      message("simulation current_life_table SOA ", length(current_life_table))
    }else{
      warning("selected_tab is not HLD or SOA: ", selected_tab)
    }
    
    return(current_life_table)
  })
  
  # input ui ----
  is_deffereal <- reactive({
    ifelse(input$payment == "Deferred", 1, 0)
  })
  
  omega_technical <- reactive({
    message("calculating omega_technical")
    message("omega_technical ", length(life_table_technical()))
    length(life_table_technical())
  })
  
  # set max age to the maximum age of the technical table
  output$age <- renderUI({
    sliderInput("age",
                "Age",
                value = 20,
                min = 0,
                max = initial_input$max_age)
  })
  
  output$duration_annuities <- renderUI({
    sliderInput("duration_annuities",
                "Duration of annuities",
                min = input$age + is_deffereal(),
                max = omega_technical() + is_deffereal(),
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

  observeEvent(input$technical_country_soa, {
    if (!is.null(input$technical_table_soa))
    {
      initial_input$technical_table_soa_value <- 
        mapping_soa_tables[[input$technical_country_soa]][1]
      updateSelectInput(session, "technical_table_soa", selected = initial_input$technical_table_soa_value)
      message("values updated from technical_country_soa ", initial_input$technical_table_soa_value)
    }else{
      message("input$technical_table_soa is null")
    }
    
    initial_input$max_age <- omega_technical()
  })
  
  observeEvent(input$simulation_country_soa, {
    if (!is.null(input$simulation_table_soa))
    {
      initial_input$simulation_table_soa_value <- 
        mapping_soa_tables[[input$simulation_table_soa]][1]
    }
  })
  
  observeEvent(input$technical_country_HLD, {
    if (!is.null(input$technical_year_table))
    {
      initial_input$technical_year_table_value <- 
        mapping_life_tables[[input$technical_country_HLD]][1]
    }
    
    initial_input$max_age <- omega_technical()
  })
  
  observeEvent(input$simulation_country_HLD, {
    if (!is.null(input$simulation_year_table))
    {
      initial_input$simulation_year_table_value <- 
        mapping_life_tables[[input$simulation_country_HLD]][1]
    }
  })
  
  observeEvent(input$age, {
    if (!is.null(input$duration_annuities))
    {
      initial_input$duration_annuities_values[1] <- 
        max(input$age + is_deffereal(), input$duration_annuities[1])
      initial_input$duration_annuities_values[2] <- 
        min(omega_technical() + is_deffereal(), input$duration_annuities[2])
    }
    })

  observeEvent(input$duration_annuities, {
    if (!is.null(input$number_premium))
    {
      initial_input$number_premiums_value <- 
        min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
    }
    
    if (!is.null(input$guaranteed_rates_duration))
    {
      initial_input$guaranteed_rates_duration_value <- 
        min(input$guaranteed_rates_duration, input$duration_annuities[2] - input$duration_annuities[1])
    }
  })
  
  observeEvent(input$payment, {
    if (!is.null(input$duration_annuities) && !is.null(input$number_premiums))
    {
      initial_input$number_premiums_value <- 
        min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
    }
    
    if (!is.null(input$duration_annuities))
    {
      initial_input$duration_annuities_values[1] <- 
        max(input$age + is_deffereal(), input$duration_annuities[1])
      initial_input$duration_annuities_values[2] <- 
        min(omega_technical() + is_deffereal(), input$duration_annuities[2])
    }
    
  })

  # render data 
  data_fund <- reactive({
    # message(paste("input active tab", input$active_tab))
    # message(
    #   paste0("data_fund \n",
    #          "number_insured: ", input$number_insured, "\n",
    #          "age: ", input$age, "\n",
    #          "annuity: ", input$annuity, "\n",
    #          "initial_fund: ", input$initial_fund, "\n",
    #          "number_premiums: ", input$number_premiums, "\n",
    #          "deffered: ", input$duration_annuities[1] - input$age, "\n",
    #          "payment: ", input$payment, "\n",
    #          "coverage_years: ", input$duration_annuities[2] - input$age, "\n",
    #          "guaranteed_rates_duration: ", input$guaranteed_rates_duration, "\n",
    #          "fund_return_rate: ", input$interest_rate, "\n",
    #          "technical_rate: ", input$technical_rate, "\n",
    #          "aleatory_rate: ", input$aleatory_rate, "\n",
    #          "mortality_table: ", ifelse(input$active_tab == "HLD", 
    #                                      paste(input$technical_country_HLD, input$technical_year_table, input$technical_sex_table),
    #                                      input$technical_table),
    #          "\n",
    #          "simulation_table: ", ifelse(input$active_tab == "HLD", 
    #                                      paste(input$simulation_country_HLD, input$simulation_year_table, input$simulation_sex_table),
    #                                      input$simulation_table),
    #          "\n",
    #          "aleatory_mortality: ", input$aleatory_mortality, "\n"
    #         )
    # )
    fund(
      number_insured = input$number_insured,
      age = input$age,
      annuity = input$annuity,
      initial_fund = input$initial_fund,
      number_premiums = input$number_premiums,
      deffered = input$duration_annuities[1] - input$age,
      payment = input$payment,
      coverage_years = input$duration_annuities[2] - input$age,
      guaranteed_rates_duration = input$guaranteed_rates_duration,
      fund_return_rate = input$interest_rate,
      technical_rate = input$technical_rate,
      aleatory_rate = input$aleatory_rate,
      mortality_table = life_table_technical(),
      simulation_table = life_table_simulation(),
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