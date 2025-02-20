plot_fund_performance <- function(f){
  ggplot(f, aes(age)) +
    geom_line(aes(y = fund), linetype = "dotted") +
    # geom_line(aes(y = fund_t), linetype = "dotted", color = "#AFFFAF") +
    geom_point(aes(y = fund)) +
    geom_area(mapping = aes(age, fund, fill = period),
              data = data_for_area(f, "fund"),
              alpha = .5,
              position = position_dodge(width = 0)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "tomato"
    ) +
    scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
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
}

# plot_fund_performance(fund())

plot_fund_theoretical <- function(f){
ggplot(f, aes(age)) +
    geom_line(aes(y = fund_t), linetype = "dotted") +
    geom_point(aes(y = fund_t)) +
    geom_area(mapping = aes(age, fund_t, fill = period),
              data = data_for_area(f, "fund_t"),
              alpha = .5,
              position = position_dodge(width = 0)) +
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
}

data_for_area <- function(f, y){
  f |>
    summarise(
      # y,
      across(age, c(min = min, max = max), .names = "age_{.fn}"),
      .by = period
    ) |>
    mutate(
      age_min = lag(age_max, default = min(age_min)),
    ) |>
    pivot_longer(
      cols = starts_with("age"),
      names_to = "limits",
      values_to = "age"
    ) |>
    left_join(f |> select(age, all_of(y)), by = c("age")) |> 
    full_join(f |> select(age, all_of(y), period), by = c("age", "period", y))
}

# data_for_area(f, "fund_t") |> 
#   arrange(age) |> 
#   print(n = 100)
# 
# 
# data_for_area(fund(), y = "fund_t")

# plot_fund_theoretical(fund())

plot_fund_spin <- function(f){
  ggplot(f, aes(age)) +
    geom_crossbar(aes(ymin = -fund_neg, ymax = fund_pos, y = fund_pos - fund_neg, fill = period), width = 0.5) +
    # geom_line(aes(y = fund_t), linetype = "dotted") +
    # geom_point(aes(y = fund_t)) +
    # geom_area(aes(y = fund_t, fill = period), alpha = 0.5, position = "stack") +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "tomato"
    ) +
    scale_y_continuous(labels = \(x) number(x, prefix = "€", scale_cut = cut_short_scale())) +
    scale_fill_manual(
      values = c("Premium" = "steelblue", "Deffered" = "olivedrab3", "Guaranteed" = "darkorange", "Coverage" = "firebrick"),
    ) +
    labs(title = "Income and Outcome of the Fund",
         x = "Age",
         y = "Volume",
         fill = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# plot_fund_spin(fund())


plot_deaths <- function(f){
  
  delta_survived <-
    full_join(
      data_for_area(f, "survived_t"),
      data_for_area(f, "survived_i"),
      by = c("period", "limits", "age")
    ) |> 
    rowwise() |> 
    mutate(
      survived_min = min(survived_t, survived_i),
      survived_delta = abs(survived_t - survived_i)
    )
  
  f |> 
    select(age, survived_i, survived_t) |> 
    pivot_longer(c(survived_i, survived_t)) |> 
    ggplot(aes(age, y = value)) +
    geom_line(aes(linetype = name), linewidth = 1.2) +
    geom_area(mapping = aes(x = age, y = survived_min, fill = period),
              data = delta_survived,
              alpha = .5,
              position = position_dodge(width = 0)) +
    geom_ribbon(aes(ymin = survived_min, ymax = survived_min + survived_delta, y = NULL),
                data = delta_survived,
                fill = "gold1",
                alpha = .4
    ) +
    scale_fill_manual(
      values = c(
        "Premium" = "steelblue",
        "Deffered" = "olivedrab3",
        "Guaranteed" = "darkorange",
        "Coverage" = "firebrick"
      ),
    ) +
    guides(
      fill = guide_none(),
      # fill = guide_legend(nrow = 2),
      linetype = guide_legend(nrow = 1),
    ) +
    labs(
      x = "Age",
      y = "Survived",
      title = "Insured along coverage time",
      linetype = "Deaths calculation method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}

# plot_deaths(fund())

plot_financial_rate <- function(f, technical_rate = 0.02){
  f |> 
    mutate(
      color = if_else(financial_rate < technical_rate, "Bad", "Good")
    ) |> 
  ggplot(aes(age)) +
    geom_line(aes(y = financial_rate), linetype = "dotted", color = "slategray4") +
    geom_point(aes(y = financial_rate, color = color)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "tomato"
    ) +
    geom_hline(yintercept = technical_rate,
               linetype = "dashed",
               color = "springgreen2"
    ) +
    geom_hline(yintercept = .02,
               linetype = "dashed",
               color = "gold"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = .01)) +
    labs(title = "Financial Rate over time",
         x = "Age",
         y = "Financial Rate"
    ) +
    scale_color_manual(
      values = c("Good" = "skyblue", "Bad" = "sienna")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none"
    )
}


# plot_financial_rate(f, technical_rate = .015)

plot_montecarlo <- function(mc)
{
  mc$simulations |>
  mutate(
    color = if_else(x < 0, "Negative", "Positive")
  ) |>
    ggplot(aes(x, fill = color)) +
    geom_histogram(bins = 15, alpha = .6) +
    geom_vline(xintercept = 0, color = "tomato", linetype = "dashed") +
    scale_fill_manual(
      values = c("Negative" = "sienna", "Positive" = "skyblue")
    ) +
    scale_x_continuous(labels = \(x) number(x, prefix = "€", scale_cut = cut_short_scale())) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
    ) +
    labs(
      y = "",
      x = "Final fund value"
    )
}

  # map_dbl(
  #   1:100,
  #   ~{
  #     fund(technical_rate = .02, aleatory_rate = T, aleatory_mortality = T) |>
  #     tail(1) |>
  #     pull(fund)
  #   }
  # ) |> monte_carlo() |> plot_montecarlo()
  