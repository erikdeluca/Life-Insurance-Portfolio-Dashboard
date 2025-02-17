
# function standard to make a table with DT
make_a_table <- function(pre_table){
  pre_table |> 
    rename_with(
      .fn = ~ .x |> str_replace_all("_", " ") |> str_to_title(),
      .cols = everything()
    ) |>
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        paging = TRUE,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        buttons = list(
          list(
            extend = "copy",
            text = "Copy",
            className = "btn btn-primary"
          ),
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download',
            className = "btn btn-primary"
          )
        ),
        # to get personal theme on buttons
        initComplete = JS(
          "function(settings, json) {",
          "$('.dt-button').removeClass('dt-button');",
          "}"
        ),
        pageLength = 5,
        lengthMenu = c(5, 5, 10, 20, 50, 100)
      )
    )
}

# 
table_real_fund <- function(f){
  f |> 
    select(
      age,
      fund,
      fund_return,
      fund_premium,
      premium_value,
      fund_annuity,
      survived_i,
      financial_rate,
    ) |> 
    mutate(
      across(
        c(fund, fund_return, fund_premium, premium_value, fund_annuity),
        \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
      ),
      across(
        financial_rate,
        \(x) scales::percent(x, accuracy = .01)
      ),
    ) |> 
    rename(
      "return" = "fund_return",
      "survived" = "survived_i"
    ) |> 
    make_a_table()
}

# table_real_fund(f)


table_theoretical_fund <- function(f){
  f |> 
    select(
      age,
      fund,
      fund_t,
      fund_premium_t,
      fund_annuity_t,
      survived_t,
      hPx
    ) |> 
    mutate(
      across(
        c(fund, fund_t, fund_premium_t, fund_annuity_t),
        \(x) number(x, prefix = "€", scale_cut = cut_short_scale())
      ),
      across(
        survived_t,
        \(x) number(x, accuracy = 1)
      ),
      across(
        hPx,
        \(x) scales::percent(x, accuracy = .01)
      ),
    ) |> 
    rename(
      "fund_real" = "fund",
      
    ) |> 
    rename_with(
      \(x) str_remove(x, "_t"),
    ) |> 
    make_a_table()
}

# table_theoretical_fund(f)
