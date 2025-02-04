ui <- navbarPage(
  
  # navbarMenu("Menu",
  #            tabPanel("Home", "Home"),
  #            tabPanel("About", "About"),
  #            tabPanel("Contact", "Contact"),
  #             
  # ),
  # navbarMenu("Fixed Rates"),
  # navbarMenu("Fixed Premiums"),
  title = "Life Insurance Portfolio Analysis",
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # sidebar panel for inputs ----
    sidebarPanel(
      
      
      # TIME INPUTS
      fluidRow(
        h4("Time Inputs"),
        
        # Input: advance or deferred payment
        prettyRadioButtons("advance_deferred_payment", "Advance or deferred payment",
                     choices = c("Advance", "Deferred"),
                     selected = "Deferred", inline = TRUE
                     ),
        # Input: Age
        sliderInput("age", "Age", value = 20, min = 0, max = 100),
        
        # Input: Duration of coverage
        uiOutput("duration_coverage"),
        
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
        
        # aleatory rate
        prettySwitch("aleatory_rate", "Aleatory rate",status = "primary", value = TRUE, inline = T),
        
        
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
        
        # aleatory mortality 
        prettySwitch("aleatory_mortality", "Aleatory mortality",status = "primary", value = TRUE, inline = T),
        
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
          numericInput("annuity", "Annuity", value = 12E3, min = 0),
        ),
        
        column(width = 6,
          # Input: Premium
          verbatimTextOutput("premium", T),
        ),
        
        
        
      ), # END fluidRow
      
    ), # END sidebarPanel
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # tabsetPanel for output tabs ----
      tabsetPanel(
        
        # first tab: 
        tabPanel("Fund Performance",
                 h3("Plot"),
                 plotOutput("fund_performance_plot"),
                 h3("Data"),
                 # dataTableOutput("cumulate_fund_performance_table")
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