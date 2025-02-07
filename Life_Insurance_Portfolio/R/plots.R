plot_fund_performance <- function(f){
  ggplot(f, aes(age)) +
    geom_line(aes(y = fund), linetype = "dotted") +
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
}

plot_fund_theoretical <- function(f){
  ggplot(f, aes(age)) +
    geom_line(aes(y = fund_t), linetype = "dotted") +
    geom_point(aes(y = fund_t)) +
    # geom_area(mapping = aes(age, fund_t, fill = period), data = data_for_area(f, "fund_t")) +
    geom_area(aes(y = fund_t, fill = period), alpha = 0.5, position = "stack") +
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

# data_for_area <- function(f, y){
#   f |>
#     summarise(
#       # y,
#       across(age, c(min = min, max = max), .names = "age_{.fn}"),
#       .by = period
#     ) |>
#     mutate(
#       age_max = lead(age_min, default = max(age_max)),
#     ) |> 
#     pivot_longer(
#       cols = starts_with("age"),
#       names_to = "limits",
#       values_to = "age"
#     ) |> 
#     left_join(f |> select(age, all_of(y)), by = "age")
# }
# 
# data_for_area(fund(), y = "fund_t")
# 
# # in development
# geom_area_continuos <- function(f, y){
#   geom_area(aes(y = y, fill = period), data = data_for_area(f, y), alpha = 0.5, position = "stack")
# }
# 
# ggplot() +
# geom_area_continuos(fund(), y = "fund_t")
# 
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


