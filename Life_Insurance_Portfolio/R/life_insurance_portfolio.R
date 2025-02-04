# build a function that given the input parameters, returns the following outputs:
# - volume_fund: a vector of the fund value at each year
# - fund_return: a vector of the return of the fund at each year
# - deaths: a vector of the number of deaths at each year
# - premium: the premium value
# - fund_rate: the fund rate at each year
# - premium_rate: the premium rate at each year
# - premium_rate_guaranteed: the premium rate guaranteed at each year

# deaths calculations --------------------------------------------
deaths_calculation <- function(
  number_insured = 1000,
  age = 20,
  omega = 110,
  simulation_table = lifecontingencies::demoIta$SIM02,
  aleatory_mortality = TRUE
  )
{  
  # initialize the died vector
  insured_dieds <- NULL
  # loop over the period
  for(i in age:omega)
  {
    survived <- number_insured - sum(insured_dieds)
    # the probability of death in year i conditioned on being alive at year i
    mu <- (simulation_table[i + 1] - ifelse(i > (omega - 2), 0, simulation_table[i + 2])) / 
            ifelse(i > (omega - 1), 1, simulation_table[age + 1])
    # generate the deaths from a Poisson with mean the ones that should die in theory
    # TODO: manipulate the parameters of the Poisson distribution
    deaths <- ifelse(
      aleatory_mortality,
      sum(rpois(number_insured, mu), na.rm = T),
      round(number_insured * mu)
      )
    insured_dieds <- c(insured_dieds, ifelse(deaths > survived, survived, deaths))
  }
  
  # distribute the deaths in omega along the previous years
  last_dieds <- sample(
    x = 1:(length(insured_dieds)-1),
    size = insured_dieds[length(insured_dieds)],
    prob = (insured_dieds[1:(length(insured_dieds)-1)] / sum(insured_dieds[1:(length(insured_dieds)-1)])),
    replace = T
  ) |> 
    table() |> 
    as.data.frame()
  insured_dieds[last_dieds$Var1] <- insured_dieds[last_dieds$Var1] + last_dieds$Freq
  insured_dieds[length(insured_dieds)] <- 0
  
  return(insured_dieds)
}

# deaths_calculation()

# hPx calculations --------------------------------------------
hPx <- function(h, x, omega, mortality_table)
{
  return(ifelse(h + x > omega - 1, 0, mortality_table[h + x + 1]) / mortality_table[x + 1])
}

# hPx(1, 20, 110, lifecontingencies::demoIta$SIM02)

# hAV calculations --------------------------------------------
hAV <- function(h, number_insured, deaths)
{
  return(number_insured - ifelse(h > 0, sum(deaths[1:h]), 0))
}

# hAV(1, 1000, deaths_calculation())

# premium calculations --------------------------------------------
premium <- function(
    annuity = 12000,
  age = 20,
  omega = 110,
  mortality_table = lifecontingencies::demoIta$SIM02,
  number_premiums = 15,
  deffered = 0,
  advance_deferred_payment = "Deferred",
  coverage_years = 35,
  guaranteed_rates_duration = 5,
  technical_rate = 0.02
)
{
  is_deferred <- ifelse(advance_deferred_payment == "Deferred", 1, 0)
  
  # calculate the premium
  if (guaranteed_rates_duration > 0)
  {
    guaranteed_rates <- sum((1 + technical_rate) ** -c((is_deferred + deffered):(is_deferred + guaranteed_rates_duration + deffered)))
    # shold I insert is_deffered with coverage_years?
    no_guaranteed_rates <- sum((1 + technical_rate) ** -c((deffered + is_deferred + guaranteed_rates_duration + 1):coverage_years) *
                                 hPx(c((deffered + is_deferred + 1 + guaranteed_rates_duration):coverage_years), age, omega, mortality_table))
    premium <- annuity * (guaranteed_rates + no_guaranteed_rates)
  } else
  {
    premium <- annuity * sum((1 + technical_rate) ** -(c((deffered + is_deferred):coverage_years)) * 
                             hPx(c((deffered + is_deferred):coverage_years), age, omega, mortality_table))
  }
  
  if (number_premiums > 1)
  {
    premium <- premium / 
      sum((1 + technical_rate) ** -c(0:(number_premiums - 1)) * hPx(c(0:(number_premiums - 1)), age, omega, mortality_table))
  }
    return(premium)
}

# premium(deffered = 25)


# financial rate calculations --------------------------------------------
financial_rate <- function(
  aleatory_rate = TRUE,
  financial_rate = 0.02,
  coverage_years = 35
)
{
  if(aleatory_rate)
  {
    financial_rate <- rnorm(coverage_years, mean = financial_rate, sd = 0.01)
  } else
  {
    financial_rate <- rep(financial_rate, coverage_years)
  }
  
  return(financial_rate)
}

# financial_rate()

# fund calculations --------------------------------------------

# The function should have the following signature:
fund <- function(
    number_insured = 1000,
    age = 20,
    annuity = 12000,
    initial_fund = 0,
    number_premiums = 15,
    omega = 110,
    deffered = 25,
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
  
  # set the deferred value
  is_deferred <- ifelse(advance_deferred_payment == "Deferred", 1, 0)
  
  # calculate the death vector
  deaths <- deaths_calculation(
    number_insured = number_insured,
    age = age,
    omega = omega,
    simulation_table = simulation_table,
    aleatory_mortality = aleatory_mortality
  )
  
  # calculate the premium
  premium_value <- premium(
    annuity = annuity,
    age = age,
    omega = omega,
    mortality_table = mortality_table,
    number_premiums = number_premiums,
    deffered = deffered,
    advance_deferred_payment = advance_deferred_payment,
    coverage_years = coverage_years,
    guaranteed_rates_duration = guaranteed_rates_duration,
    technical_rate = technical_rate
  )
  
  # calculate the financial rate
  financial_rates <- financial_rate(
    aleatory_rate = aleatory_rate,
    financial_rate = fund_return_rate,
    coverage_years = coverage_years
  )
  
  # initialize the fund vector
  # fund_details <-
  tibble(
    age = (age + is_deferred):(coverage_years + age + is_deferred - 1),
    fund = c(initial_fund, rep(0, length(age) - 1)),
    fund_return = rep(0, length(age)),
    deaths = deaths[1:length(age)],
    survived = number_insured - cumsum(deaths),
    fund_premium = rep(0, length(age)),
    fund_annuity = rep(0, length(age)),
    financial_rate = financial_rates,
  ) |> 
    # manipulate the fund vectors
    mutate(
      fund_premium = c(premium_value * sapply(0:(number_premiums - 1), \(h) hAV(h, number_insured, deaths)),
                       rep(0, coverage_years - number_premiums)),
      fund_annuity = c(
        rep(0, deffered + is_deferred - 1),
        rep(annuity * number_insured, guaranteed_rates_duration),
        rep(
          annuity * 
            sapply((guaranteed_rates_duration + deffered + is_deferred):(coverage_years), \(h) hPx(h, !!age, omega, mortality_table) * number_insured)
          )
        ),
      # # calculate the fund annuities
      # fund_premium_cumulated = cumsum(fund_premium * (1 + financial_rate)),
      # fund = fund * cumprod(1 + financial_rate) + fund_premium * (1 + financial_rate) - fund_annuity,
      # fund_return = if_else(lag(fund, default = initial_fund) == 0,
      #                       0,
      #                       (fund - lag(fund, default = initial_fund)) / lag(fund, default = initial_fund)
      # )
    ) -> fund_details
  
  for(i in 1:nrow(fund_details))
  {
    fund_details$fund[i] = (ifelse(i > 1, fund_details$fund[i - 1], initial_fund) + 
      fund_details$fund_premium[i]) * (1 + fund_details$financial_rate[i]) - fund_details$fund_annuity[i]
  }
  
    fund_details |> 
      mutate(
        fund_return = fund - lag(fund, default = initial_fund)
      ) -> fund_details
    
  return(fund_details)
}
# 
fund(
  # advance_deferred_payment = "Advance",
  advance_deferred_payment = "Deffered",
  # initial_fund = 1E10,
  number_insured = 1E5,aleatory_mortality = F, aleatory_rate = F) |> 
  print(n = 100)
  
fund(
  # advance_deferred_payment = "Advance",
  advance_deferred_payment = "Deffered",
  # initial_fund = 1E10,
  number_insured = 1E5,aleatory_mortality = F, aleatory_rate = F) |> 
  arrange(-age) |> 
  summarise(first(fund) / last(survived))

