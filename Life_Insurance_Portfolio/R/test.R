number_insured = 1000
annuity = 1
age = 20
omega = 120
mortality_table = demoIta$SIM02
number_premiums = 1
deffered = 0
payment = "advance"
# payment = "arrears"
coverage_years = 100
guaranteed_rates_duration = 0
technical_rate = .02
fund_return_rate = .02
aleatory_mortality = F

premium(
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

lifecontingencies::axn(
  actuarialtable = new("actuarialtable", lx = mortality_table, interest = .02, x = 0:120, name = "SIM02"),
  x = age,
  n = coverage_years,
  m = deffered,
  type = "EV",
  payment = payment
)



fund(aleatory_mortality = F, aleatory_rate = F, age = 60, guaranteed_rates_duration = 0) |> 
  print(n = 100)
fund(aleatory_mortality = F, aleatory_rate = F, age = 60, guaranteed_rates_duration = 0) |> 
  summarise(
    fund_final_per_person = last(fund) / first(survived),
    ratio_on_premium = fund_final_per_person / sum(premium_value),
    fund_final = last(fund),
    fund_return = fund_final / first(fund) - 1,
    fund_premium = sum(fund_premium),
    fund_annuity = sum(fund_annuity),
    fund_p_on_a = fund_premium / fund_annuity, # it doesn't take into account the number of insured along the years and the discount rate 
  )

fund(aleatory_mortality = F, aleatory_rate = F, age = 60, guaranteed_rates_duration = 0) |>
  add_row(
    n = 0,
    age = min(age) - 1,
    fund = initial_fund,
    fund_t = initial_fund,
    .before = 1
  ) |> 
  ggplot(aes(age)) +
  # geom_line(aes(y = fund)) +
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
# 
fund(aleatory_mortality = F, aleatory_rate = F, age = 60, guaranteed_rates_duration = 0) |>
  # print(n = 100)
  mutate(
    # across(
    #   c(fund, fund_return, fund_premium, premium_value, fund_annuity),
    #   \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
    # ),
    across(
      financial_rate,
      \(x) scales::percent(x, accuracy = .01)
    ),
  )

# discounted
c(
  sum(
    (1 + tassoTecnico)**-c(is_advance:coverage_years) * hPx(c(is_advance:coverage_years), age, omega, mortality_table)
    ),
  -hPx(c(is_advance:coverage_years), age, omega, mortality_table) * (1 + tassoTecnico)**-c(is_advance:coverage_years)
) |> sum()

# map each year
map_dfr(
  1:coverage_years,
  ~{
    c(
      year = .x,
      premium = sum(
        (1 + tassoTecnico)**-c(is_advance:coverage_years) * hPx(c(is_advance:coverage_years), age, omega, mortality_table)
      ) * (1 + tassoTecnico)**.x,
      # annuities = sum(hPx(1:.x, age, omega, mortality_table) * (1 + tassoTecnico)**c(1:.x))
      hpx = hPx(.x, age, omega, mortality_table),
      tx = (1 + tassoTecnico)**.x
    )
  }
) |> 
  mutate(
    annuity = hpx * tx,
    annuities = cumsum(annuity),
    net = premium - annuities
  ) |> 
  print(n = coverage_years)

fondo = premium(
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

for(i in 1:coverage_years)
{
  fondo = c(fondo, (fondo[i] - hPx(i - 1, age, omega, mortality_table)) * (1 + technical_rate))
}
tibble(
  year = 0:coverage_years,
  age = age + year,
  fondo,
  hpx = hPx(year, !!age, omega, mortality_table)
) |> 
  print(n = coverage_years)

fund(
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
) |> print(n = coverage_years)


# fund(
#   # payment = "Advance",
#   payment = "Deffered",
#   # initial_fund = 1E10,
#   number_insured = 1E5,aleatory_mortality = F, aleatory_rate = F) |> 
#   print(n = 100)
# 
# fund(
#   # payment = "Advance",
#   payment = "Deffered",
#   # initial_fund = 1E10,
#   number_insured = 1E5,aleatory_mortality = F, aleatory_rate = F) |> 
#   arrange(-age) |> 
#   summarise(first(fund) / last(survived))
