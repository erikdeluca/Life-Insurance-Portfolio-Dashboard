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

# sum(deaths_calculation())

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
  payment = "advance",
  coverage_years = 35,
  guaranteed_rates_duration = 5,
  technical_rate = 0.02
)
{
  is_advance <- ifelse(payment == "advance", 1, 0)
  
  # calculate the premium
  if (guaranteed_rates_duration > 0)
  {
    guaranteed_rates <- sum((1 + technical_rate) ** -c((1 - is_advance + deffered):(-is_advance + guaranteed_rates_duration + deffered)))
    # shold I insert is_deffered with coverage_years?
    no_guaranteed_rates <- sum((1 + technical_rate) ** -c((1 - is_advance + deffered + guaranteed_rates_duration):coverage_years) *
                                 hPx(c((1 - is_advance + deffered + guaranteed_rates_duration):coverage_years), age, omega, mortality_table))
    premium <- annuity * (guaranteed_rates + no_guaranteed_rates)
  } else
  {
    premium <- annuity * sum((1 + technical_rate) ** -(c((deffered - is_advance + 1):(coverage_years - is_advance))) * 
                             hPx(c((deffered - is_advance + 1):(coverage_years - is_advance)), age, omega, mortality_table))
  }
  
  if (number_premiums > 1)
  {
    if (guaranteed_rates_duration > 0)
    {
      # premiums_no_guaranteed_rates
      premium <- rep(annuity * no_guaranteed_rates / 
        sum((1 + technical_rate) ** -c(0:(number_premiums - 1)) * hPx(c(0:(number_premiums - 1)), age, omega, mortality_table)),
        number_premiums)
      
      # first premium with guaranteed rates
      premium[1] <- premium[1] + annuity * guaranteed_rates
    }else
    {
      premium <- rep(premium / 
        sum((1 + technical_rate) ** -c(0:(number_premiums - 1)) * hPx(c(0:(number_premiums - 1)), age, omega, mortality_table)),
        number_premiums)
    }
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
    payment = "advance",
    coverage_years = 35,
    guaranteed_rates_duration = 5,
    fund_return_rate = 0.02,
    technical_rate = 0.02,
    aleatory_rate = TRUE,
    mortality_table = lifecontingencies::demoIta$SIM02,
    simulation_table = lifecontingencies::demoIta$SIM02,
    aleatory_mortality = TRUE
) {
  
  # set the advance value
  is_advance <- ifelse(payment == "advance", 1, 0)

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
    payment = payment,
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
  last_survived <- number_insured - sum(deaths[1:coverage_years + is_advance])
  last_survived_t <- number_insured * hPx(coverage_years + is_advance, age, omega, mortality_table)
  # fund_details <-
  tibble(
    n = is_advance:(coverage_years + is_advance - 1),
    age = age:(coverage_years + age - 1),
    fund = c(initial_fund, rep(0, length(age) - 1)),
    # fund_return = rep(0, length(age)),
    deaths = c(0, deaths[1:(length(age) - 1)]),
    survived_i = number_insured - cumsum(deaths) + deaths,
    survived_f = number_insured - cumsum(deaths),
    # theoretical survived
    premium_value = c(premium_value, rep(0, length(age) - number_premiums)),
    fund_premium = rep(0, length(age)),
    fund_annuity = rep(0, length(age)),
    financial_rate = financial_rates,
    fund_t = c(initial_fund, rep(0, length(age) - 1)),
    hPx = hPx(n - is_advance, !!age, omega, mortality_table),
    survived_t = number_insured * hPx,
  ) |> 
    # manipulate the fund vectors
    mutate(
      fund_premium = premium_value * survived_i,
      fund_premium_t = premium_value * survived_t,
      fund_annuity = case_when(
        n < deffered + is_advance ~ 0,
        n < guaranteed_rates_duration + deffered + is_advance ~ annuity * number_insured,
        (n < coverage_years + is_advance + 1) & is_advance == 1 ~ survived_f * annuity,
        (n < coverage_years + is_advance + 1) & is_advance == 0 ~ lead(survived_f, default = last_survived) * annuity,
        TRUE ~ NA_real_  # Use NA for undefined cases
      ),
      fund_annuity_t = case_when(
        n < deffered + is_advance ~ 0,
        n < guaranteed_rates_duration + deffered + is_advance ~ annuity * number_insured,
        (n < coverage_years + is_advance) & is_advance == 0 ~ lead(survived_t, default = last_survived_t) * annuity,
        (n < coverage_years + is_advance) & is_advance == 1 ~ survived_t * annuity,
        TRUE ~ NA_real_  # Use NA for undefined cases
      ),
      period = case_when(
        n < number_premiums + is_advance ~ "Premium",
        n < deffered + is_advance ~ "Deffered",
        n < guaranteed_rates_duration + deffered + is_advance ~ "Guaranteed",
        n < coverage_years + is_advance ~ "Coverage",
        TRUE ~ NA_character_  # Use NA for undefined cases
      ) |> 
        factor(levels = c("Premium", "Deffered", "Guaranteed", "Coverage"))
      # # calculate the fund annuities
      # fund_premium_cumulated = cumsum(fund_premium * (1 + financial_rate)),
      # fund = fund * cumprod(1 + financial_rate) + fund_premium * (1 + financial_rate) - fund_annuity,
      # fund_return = if_else(lag(fund, default = initial_fund) == 0,
      #                       0,
      #                       (fund - lag(fund, default = initial_fund)) / lag(fund, default = initial_fund)
      # )
    # ) |> print(n = 100)  # for DEBUGGING
    ) -> fund_details
  
  for(i in 1:nrow(fund_details))
  {
    
    fund_details$fund[i] = (ifelse(i > 1, fund_details$fund[i - 1], initial_fund) + 
      fund_details$fund_premium[i]) * (1 + fund_details$financial_rate[i]) - 
      fund_details$fund_annuity[i] * (1 + fund_details$financial_rate[i])**is_advance # when is_advance = 0, the fund_annuity isn't capitalized
    
    # fund theoretical take only the theoretical values, so initial_fund is 0 and the financial_rate is the technical_rate
    fund_details$fund_t[i] = (ifelse(i > 1, fund_details$fund_t[i - 1], 0) +
      fund_details$fund_premium_t[i]) * (1 + technical_rate) -
      fund_details$fund_annuity_t[i] * (1 + technical_rate)**is_advance # when is_advance = 0, the fund_annuity isn't capitalized

  }
  
  fund_details |> 
    mutate(
      fund_return = fund - lag(fund, default = initial_fund),
      fund_pos = (lag(fund, default = initial_fund) + fund_premium) * (1 + financial_rate) - lag(fund, default = initial_fund),
      fund_neg = fund_annuity * (1 + financial_rate) ** (1 - is_advance),
    ) -> fund_details
    
  return(fund_details)
}


number_insured = 1000
annuity = 1
age = 20
omega = 120
mortality_table = demoIta$SIM02
number_premiums = 10
deffered = 35
payment = "advance"
# payment = "arrears"
coverage_years = 100
guaranteed_rates_duration = 10
technical_rate = .02
fund_return_rate = .02
aleatory_mortality = F

f <- fund(
  number_insured = number_insured,
  age = age,
  annuity = annuity,
  initial_fund = 0,
  number_premiums = number_premiums,
  omega = omega,
  deffered = deffered,
  payment = payment,
  coverage_years = coverage_years,
  guaranteed_rates_duration = guaranteed_rates_duration,
  fund_return_rate = fund_return_rate,
  technical_rate = technical_rate,
  aleatory_rate = F,
  aleatory_mortality = F,
  mortality_table = demoIta$SIM02,
  simulation_table = demoIta$SIM02
) 

print(f, n = coverage_years)

# p <-
ggplot(f, aes(age)) +
  # geom_line(aes(y = fund)) +
  # geom_line(aes(y = fund_t), linetype = "dotted", color = "#AFFFAF") +
  geom_point(aes(y = fund)) +
  geom_area(aes(y = fund, fill = period), alpha = 0.5, position = "stack") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "tomato"
  ) +
  scale_y_continuous(labels = \(x) number(x, prefix = "€", scale_cut = cut_short_scale())) +
  scale_fill_manual(
    values = c("Premium" = "steelblue", "Deffered" = "olivedrab3", "Guaranteed" = "darkorange", "Coverage" = "firebrick"),
  ) +
  labs(title = "Fund value over time",
       x = "Age",
       y = "Fund value",
       fill = ""
       ) +
  theme_minimal() +
  theme(legend.position = "bottom")

