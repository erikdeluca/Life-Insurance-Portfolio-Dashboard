
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
  ggplot(aes(age, fund)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Fund value over time",
       x = "Age",
       y = "Fund value") +
  theme_minimal()
# 
fund(aleatory_mortality = F, aleatory_rate = F, age = 60, guaranteed_rates_duration = 0) |>
  # print(n = 100)
  mutate(
    across(
      c(fund, fund_return, fund_premium, premium_value, fund_annuity),
      \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
    ),
    across(
      financial_rate,
      \(x) scales::percent(x, accuracy = .01)
    ),
  )

