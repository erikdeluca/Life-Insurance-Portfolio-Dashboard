
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


