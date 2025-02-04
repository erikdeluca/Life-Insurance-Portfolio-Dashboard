fund(number_insured = 1E5,aleatory_mortality = F, aleatory_rate = F) |> 
  arrange(-age) |> 
  summarise(first(fund) / last(survived))
  
fund(aleatory_mortality = F, aleatory_rate = F) |> 
  ggplot(aes(age, fund)) +
  geom_line() +
  geom_point() +
  labs(title = "Fund value over time",
       x = "Age",
       y = "Fund value") +
  theme_minimal()
