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
    number_premiums_value = 1,
    guaranteed_rates_duration_value = 0,
    duration_annuities_values = c(0,0)
    )
  
  observeEvent(input$age, {
    initial_input$duration_annuities_values[1] <- 
      max(input$age + is_deffereal(), input$duration_annuities[1])
    initial_input$duration_annuities_values[2] <- 
      min(omega() + is_deffereal(), input$duration_annuities[2])
    
    initial_input$number_premiums_value <- 
      min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
  })
  
  observeEvent(input$duration_annuities, {
    initial_input$number_premiums_value <- 
      min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
    
    initial_input$guaranteed_rates_duration_value <- 
      min(input$guaranteed_rates_duration, input$duration_annuities[2] - input$duration_annuities[1])
  })
  
  observeEvent(input$payment, {
    initial_input$duration_annuities_values[1] <- 
      max(input$age + is_deffereal(), input$duration_annuities[1])
    initial_input$duration_annuities_values[2] <- 
      min(omega() + is_deffereal(), input$duration_annuities[2])
    
    initial_input$number_premiums_value <- 
      min(input$number_premiums, input$duration_annuities[1] - input$age + is_deffereal())
  })
  
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
  
  # render data 
  data_fund <- reactive({
    message(
      paste0("data_fund \n",
             "number_insured: ", input$number_insured, "\n",
             "age: ", input$age, "\n",
             "annuity: ", input$annuity, "\n",
             "initial_fund: ", input$initial_fund, "\n",
             "number_premiums: ", input$number_premiums, "\n",
             "omega: ", omega(), "\n",
             "deffered: ", input$duration_annuities[1] - input$age, "\n",
             "payment: ", input$payment, "\n",
             "coverage_years: ", input$duration_annuities[2] - input$age, "\n",
             "guaranteed_rates_duration: ", input$guaranteed_rates_duration, "\n",
             "fund_return_rate: ", input$interest_rate, "\n",
             "technical_rate: ", input$technical_rate, "\n",
             "aleatory_rate: ", input$aleatory_rate, "\n",
             "mortality_table: ", input$technical_table, "\n",
             "simulation_table: ", input$simulation_table, "\n",
             "aleatory_mortality: ", input$aleatory_mortality, "\n"
            )
    )
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
      ggplot(aes(age)) +
      geom_line(aes(y = fund)) +
      geom_line(aes(y = fund_t), linetype = "dotted", color = "#AFFFAF") +
      geom_point(aes(y = fund)) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 color = "tomato"
                 ) +
      labs(title = "Fund value over time",
           x = "Age",
           y = "Fund value") +
      theme_minimal()
  })
  
  output$fund_performance_table <- renderDataTable({
    data_fund() |> 
      arrange(-n) |> 
      mutate(
        across(
          financial_rate,
          \(x) scales::percent(x, accuracy = .01)
        ),
        # across(
        #   c(fund, fund_return, fund_premium, premium_value, fund_annuity),
        #   \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
        # ),
        across(
          where(is.numeric),
          round
        ),
      )
    
  })
  

}