# build a function that given the input parameters, returns the following outputs:
# - volume_fund: a vector of the fund value at each year
# - fund_return: a vector of the return of the fund at each year
# - deaths: a vector of the number of deaths at each year
# - premium: the premium value
# - fund_rate: the fund rate at each year
# - premium_rate: the premium rate at each year
# - premium_rate_guaranteed: the premium rate guaranteed at each year

# The function should have the following signature:
life_insurance_portfolio <- function(
  number_insured = 1000,
  age = 20,
  rates = 12000,
  initial_fund = 0,
  number_premiums = 15,
  omega = 110,
  advance_deferred_payment = "Deferred",
  coverage_years = 35,
  guaranteed_rates_duration = 5,
  fund_return_rate = 0.02,
  technical_rate = 0.02,
  aleatory_rate = TRUE,
  mortality_table = lifecontingencies::demoIta$SIM02,
  simulation_table = lifecontingencies::demoIta$SIM02,
  aleatory_mortality = TRUE
) {
    
}

# deaths calculations
