# map_dfc(
#   1:1e1,
#   ~{
#     x <- c(.x = deaths_calculation(age = age, omega = omega))
#   }
# ) |> 
#   bind_cols(age = age:omega,
#   ) |>
#   pivot_longer(-age) |> 
#   ggplot(aes(age, value, group = name)) +
#   geom_smooth(color = "tomato", se = F, alpha = .5) +
#   # geom_line(aes(y = hPx * number_insured), linetype = "dashed") +
#   # geom_area(alpha = .5, fill = "tomato") +
#   theme_minimal()
# 
