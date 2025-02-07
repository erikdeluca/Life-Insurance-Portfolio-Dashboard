ui <- fluidPage(
  
  # add css file ----
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "sass-styles.css"),
  #   tags$title("Life Insurance Portfolio Analysis")
  # ),
  
  
  useShinyjs(),
  
  title = "Life Insurance Portfolio Analysis",
  titlePanel("Life Insurance Portfolio Analysis", windowTitle = "Life Insurance Portfolio Analysis"),
  
  # use CSS file
  theme = "www/sass-styles.css",

  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    
    # sidebar panel for inputs ----
    div( id ="Sidebar",sidebarPanel(
      
      
      # TIME INPUTS
      fluidRow(
        h4("Time Inputs"),
        
        # Input: advance or deferred payment
        prettyRadioButtons("payment", "Advance (due) or arrears (immediate) payments",
                     choices = c("advance", "arrears"),
                     selected = "advance", inline = TRUE
                     ),
        # Input: Age
        sliderInput("age", "Age", value = 20, min = 0, max = 100),
        
        # Input: Duration of coverage
        uiOutput("duration_annuities"),
        
        # Input: Number of premiums
        uiOutput("number_premiums"),
        
        # Input: guaranteed rates duration
        uiOutput("guaranteed_rates_duration"),
      
      ), # END fluidRow
      
      # RATES AND MORTALITY INPUTS
      fluidRow(
      h4("Rates and Mortality Inputs"),
        
        # Interest
        column(width = 6,
          # interest rate input
          numericInput("interest_rate", "Interest rate", value = 0.02, min = 0, max = 1, step = 0.01),
        ),
        
        column(width = 6,
          # technical rate input
          numericInput("technical_rate", "Technical rate", value = 0.02, min = 0, max = 1, step = 0.01),
        ),
        
        # Mortality
        column(width = 6,
          # Input: technical table
          selectInput("technical_table",
                      "Technical table",
                      choices = names(demoIta),
                      selected = names(demoIta)[6]),
        ),
  
        column(width = 6,
          # Input: simulation table
          selectInput("simulation_table",
                      "Simulation table",
                      choices = names(demoIta),
                      selected = names(demoIta)[6]),
        ),
      ), # END fluidRow
      
      fluidRow(
        h4("Aleatory"),
        
        # aleatory rate
        prettySwitch("aleatory_rate", "Aleatory rate",status = "primary", value = F),
        
        
        
        # aleatory mortality 
        prettySwitch("aleatory_mortality", "Aleatory mortality",status = "primary", value = F),
        
      ), # END fluidRow
      
      # FUNDS INPUTS
      fluidRow(
        h4("Funds Inputs"),
        
        column(width = 6,
          # Input: Initial fund
          numericInput("initial_fund", "Initial fund", value = 0, min = 0),
        ),
        
        column(width = 6,
          # Input: Number of insured
          numericInput("number_insured", "Number of insured", value = 1000),
        ),
        
        column(width = 6,
          # Input: Rate
          numericInput("annuity", "Annuity", value = 1, min = 0),
        ),
        
        column(width = 6,
          # Input: Premium
          verbatimTextOutput("premium", T),
        ),
        
        
        
      ), # END fluidRow
      
    )), # END sidebarPanel
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      actionButton("toggleSidebar", "Toggle sidebar"),
      
      # tabsetPanel for output tabs ----
      tabsetPanel(
        
        
        # first tab: 
        tabPanel("Fund Performance",
                 
                 h3("Plot"),
                 plotOutput("fund_performance_plot") |> 
                   withSpinner(),
                 
                 h3("Data"),
                 tabsetPanel(
                   tabPanel("Real Fund",
                            dataTableOutput("real_fund_table") |> 
                              withSpinner()
                   ),
                   tabPanel("Theoretical Fund",
                            dataTableOutput("theoretical_fund_table") |> 
                              withSpinner()
                   )
                 )
        ), # END first tabPanel
        
        # second tab
        tabPanel("Yearly Fund Performance",
                 h3("Plot"),
                 # plotOutput("cumulate_fund_performance_plot"
                 #            
                 #            ),
                 h3("Data"),
                 # dataTableOutput("cumulate_fund_performance_table")
                 
        ), # END second tabPanel
        
        # third tab
        tabPanel("Death Rate",
                 h3("Plot"),
                 # plotOutput("cumulate_fund_performance_plot"
                 #            
                 #            ),
                 h3("Data"),
                 # dataTableOutput("cumulate_fund_performance_table")
        ), # END third tabPanel
        
        # fourth tab
        tabPanel("Monte Carlo Simulation",
                 h3("Plot"),
                 # plotOutput("cumulate_fund_performance_plot"
                 #            
                 #            ),
                 h3("Data"),
                 
                 # dataTableOutput("cumulate_fund_performance_table")
        ) # END fourth tabPanel
      ) # END tabsetPanel
      
    ) # END mainPanel
    
  ) # END sidebarLayout
)