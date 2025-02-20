library(tidyverse)

monte_carlo <- function(x, level = 0.95, num_moments = 10) {
  
  n_simulations <- length(x)
  expected_value <- mean(x)
  sample_variance <- sd(x) / sqrt(n_simulations - 1)
  
  z <- qnorm((1 - level) / 2, lower.tail = FALSE)
  conf_interval_low <- expected_value - z * sample_variance
  conf_interval_up <- expected_value + z * sample_variance
  
  moments <- map_dbl(1:num_moments, ~mean(x ^ .))
  
  ruin_probability <- sum(x < 0) / n_simulations
  
  results_table <- tibble(
    expected_value = expected_value,
    sample_variance = sample_variance,
    conf_interval_lower = conf_interval_low,
    conf_interval_upper = conf_interval_up,
    ruin_probability = ruin_probability
  )
  
  simulations <- tibble(
    x = x
  )
  
  return(list(
    expected_value = expected_value,
    sample_variance = sample_variance,
    conf_interval = c(conf_interval_low, conf_interval_up),
    moments = moments,
    ruin_probability = ruin_probability,
    results_table = results_table,
    simulations = simulations
  ))
}

# {
#   set.seed(1)
#   n_simulations <- 100
#   pb <- txtProgressBar(min = 0, max = n_simulations, initial = 0, title = "Monte Carlo Simulation")
#   map_dbl(
#     1:n_simulations,
#     ~{
#       setTxtProgressBar(pb,.x,
#                         label = paste0("Generating the simulation n. ", .x, " of ", n_simulations))
#       fund(technical_rate = .016, aleatory_rate = T, aleatory_mortality = T) |>
#       tail(1) |>
#       pull(fund)
#     }
#   ) |>
#     monte_carlo() ->mc
#     # monte_carlo() |>
#     # print()
#   close(pb)
# }
