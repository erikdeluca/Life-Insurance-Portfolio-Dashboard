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
    
    selected_tab <- ifelse(!is.null(input$active_tab), input$active_tab, "Human Life-Table Database")
    
    if (selected_tab == "Human Life-Table Database") {
      message(paste("technical year", input$technical_year_table))
      current_life_table <- life_tables |>
        filter(
          country == input$technical_country_HLD,
          year == ifelse(!is.null(input$technical_year_table), input$technical_year_table, initial_input$technical_year_table_value),
          sex == input$technical_sex_table
        ) |> pull(l_x)
    } else if (selected_tab == "Society Of Actuaries") {
      message("input$technical_table_soa ", input$technical_table_soa)
      message("input$technical_country_soa ", input$technical_country_soa)
      selected_soa_table <- initial_input$technical_table_soa_value
      current_life_table <- soa_tables[[input$technical_country_soa]] |>
        filter(selected_soa_table != 0) |> 
        pull(selected_soa_table)
    }else{
      warning("selected_tab is not Human Life-Table Database or Society Of Actuaries: ", selected_tab)
    }
    
    return(current_life_table)
  })
  
  life_table_simulation <- reactive({
    selected_tab <- ifelse(!is.null(input$active_tab), input$active_tab, "Human Life-Table Database")
    
    if (selected_tab == "Human Life-Table Database") {
      current_life_table <- life_tables |>
        filter(
          country == input$simulation_country_HLD,
          year == ifelse(!is.null(input$simulation_year_table), input$simulation_year_table, initial_input$simulation_year_table_value),
          sex == input$simulation_sex_table
        ) |> pull(l_x)
    } else if (selected_tab == "Society Of Actuaries") {
      selected_soa_table <- ifelse(!is.null(input$simulation_table_soa), input$simulation_table_soa, initial_input$simulation_table_soa_value)
      current_life_table <- soa_tables[[input$simulation_country_soa]] |> 
        filter(selected_soa_table != 0) |> 
        pull(selected_soa_table)
      message("simulation current_life_table SOA ", length(current_life_table))
    }else{
      warning("selected_tab is not Human Life-Table Database or SOA: ", selected_tab)
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
    sliderInput("age", "Età", 
                min = ifelse(is.null(initial_input$min_age), 0, initial_input$min_age),
                max = ifelse(is.null(initial_input$max_age), 100, initial_input$max_age),
                value = ifelse(is.null(input$age), 30, input$age))
    
  })
  
  
  output$duration_annuities <- renderUI({
    min_val <- ifelse(is.null(input$age), 30, input$age) + is_deffereal()
    max_val <- ifelse(is.null(omega_technical()), initial_input$max_age, omega_technical()) + is_deffereal()
    sliderInput("duration_annuities",
                "Duration of annuities",
                min = min_val,
                max = max_val,
                value = initial_input$duration_annuities_values,
                step = 1
    )
  })
  
  
  output$number_premiums <- renderUI({
    min_val <- 1
    max_val <- ifelse(is.null(input$duration_annuities) || is.null(input$age), 30, 
                      input$duration_annuities[1] - input$age + is_deffereal())
    sliderInput("number_premiums",
                "Number of premiums",
                min = min_val,
                max = max_val,
                value = initial_input$number_premiums_value,
                step = 1
    )
  })
  

  output$guaranteed_rates_duration <- renderUI({
    min_val <- 0
    max_val <- ifelse(is.null(input$duration_annuities) || length(input$duration_annuities) < 2, 
                      initial_input$max_age, 
                      max(0, input$duration_annuities[2] - input$duration_annuities[1]))
    sliderInput("guaranteed_rates_duration",
                "Duration of guaranteed rates",
                min = min_val,
                max = max_val,
                value = min(max_val, ifelse(is.null(input$guaranteed_rates_duration), 5, input$guaranteed_rates_duration)),
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
    # initial_input$age_value <- ifelse(input$age > initial_input$max_age, initial_input$max_age, input$age)
  })
  
  observeEvent(input$technical_country_soa, {
    if (!is.null(mapping_soa_tables[[input$technical_country_soa]]))
    {
      initial_input$technical_table_soa_value <- mapping_soa_tables[[input$technical_country_soa]][1]
      updateSelectInput(session, "technical_table_soa", selected = initial_input$technical_table_soa_value)
    }
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

    initial_input$age_value <- ifelse(input$age > initial_input$max_age, initial_input$max_age, input$age)
    initial_input$max_age <- omega_technical()
  })
  
  observeEvent(input$technical_year_table, {
    if (!is.null(input$age))
    {
      initial_input$max_age <- omega_technical()
      initial_input$age_value <- 
        ifelse(input$age > initial_input$max_age, initial_input$max_age, input$age)
    }
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
    #          "mortality_table: ", ifelse(input$active_tab == "Human Life-Table Database", 
    #                                      paste(input$technical_country_HLD, input$technical_year_table, input$technical_sex_table),
    #                                      input$technical_table),
    #          "\n",
    #          "simulation_table: ", ifelse(input$active_tab == "Human Life-Table Database", 
    #                                      paste(input$simulation_country_HLD, input$simulation_year_table, input$simulation_sex_table),
    #                                      input$simulation_table),
    #          "\n",
    #          "aleatory_mortality: ", input$aleatory_mortality, "\n"
    #         )
    # )
    
    # validate function
    validate(
      need(input$number_insured > 0, "Number of insured must be greater than 0"),
      need(input$age > 0, "Age must be greater than 0"),
      need(input$annuity > 0, "Annuity must be greater than 0"),
      need(input$initial_fund >= 0, "Initial fund must be greater or equal to 0"),
      need(input$number_premiums > 0, "Number of premiums must be greater than 0"),
      need(input$duration_annuities[1] > 0, "Duration of annuities must be greater than 0"),
      need(input$duration_annuities[2] > 0, "Duration of annuities must be greater than 0"),
      need(input$duration_annuities[2] > input$duration_annuities[1], "Duration of annuities must be greater than duration of coverage"),
      need(input$guaranteed_rates_duration >= 0, "Duration of guaranteed rates must be greater or equal to 0"),
      need(input$interest_rate >= 0, "Interest rate must be greater or equal to 0"),
      need(input$technical_rate >= 0, "Technical rate must be greater or equal to 0"),
      need(input$aleatory_rate >= 0, "Aleatory rate must be greater or equal to 0"),
      need(input$aleatory_rate <= 1, "Aleatory rate must be less or equal to 1"),
      need(input$aleatory_mortality %in% c(TRUE, FALSE), "Aleatory mortality must be TRUE or FALSE"),
      need(input$payment %in% c("advance", "arrears"), "Payment must be Advance or Deferred"),
      need(input$active_tab %in% c("Human Life-Table Database", "Society Of Actuaries"), "Active tab must be Human Life-Table Database or Society Of Actuaries"),
    )
    
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
    req(data_fund())
    data_fund() |> 
      plot_fund_performance()
  })
  
  output$fund_spin_plot <- renderPlot({
    req(data_fund())
    data_fund() |> 
      plot_fund_spin()
  })
  
  output$real_fund_table <- renderDataTable({
    req(data_fund())
    table_real_fund(data_fund())
    })
  
  # FUND THEORETICAL tabset ----
  output$theoretical_fund_plot <- renderPlot({
    req(data_fund())
    data_fund() |> 
      plot_fund_theoretical()
  })
  
  output$theoretical_fund_table <- renderDataTable({
    req(data_fund())
    table_theoretical_fund(data_fund())
  })
  
  # DEATHS AND FINANCIAL RATES tabset ----
  
  output$deaths_plot <- renderPlot({
    req(data_fund())
    plot_deaths(data_fund())
  })
  
  output$financial_rate_plot <- renderPlot({
    req(data_fund())
    plot_financial_rate(data_fund(), input$technical_rate)
  })
  
  # MONTECARLO tabset ----
  montecarlo_data <- reactive({
    set.seed(1)
    n_simulations <- 100
    pb <- txtProgressBar(min = 0, max = n_simulations, initial = 0, title = "Monte Carlo Simulation")
    map_dbl(
      1:n_simulations,
      ~{
        setTxtProgressBar(pb,.x,
                          label = paste0("Generating the simulation n. ", .x, " of ", n_simulations))
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
          mortality_table = life_table_technical(),
          simulation_table = life_table_simulation(),
          aleatory_rate = TRUE, # Otherwise montecarlo will not work
          aleatory_mortality = TRUE
        ) |>
        tail(1) |>
        pull(fund)
      }
    ) |>
      monte_carlo() -> montecarlo
      
    close(pb)
    return(montecarlo)
    
  })
  
  output$montecarlo_plot <- renderPlot({
    req(montecarlo_data())
    plot_montecarlo(montecarlo_data())
  })
  
  output$expected_value <- renderUI({
    req(montecarlo_data())
    montecarlo_data()$expected_value |> 
      number(prefix = "€", scale_cut = cut_short_scale())
  })
  
  output$ruin_value <- renderUI({
    req(montecarlo_data())
    scales::percent(montecarlo_data()$ruin_probability, accuracy = .01) 
  })

}