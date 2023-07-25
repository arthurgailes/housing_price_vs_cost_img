#' @source Home prices from Zillow: https://www.zillow.com/research/data/
#' @source Interest from St. Louis FRED: https://fred.stlouisfed.org/series/MORTGAGE30US

pacman::p_load(data.table, here, snakecase, testthat, ggplot2,
  glue, ggthemes, showtext, ggtext)

setwd(here())

font_add_google('Merriweather', 'Merriweather')
showtext_auto()
showtext_opts(dpi = 300)

vals_raw <- fread("data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
setnames(vals_raw, to_snake_case)

interest <- fread("data/MORTGAGE30US.csv")
setnames(interest, to_snake_case)

vals <- melt(
  vals_raw, 
  id.vars = c("region_id", "size_rank", "region_name", "region_type", "state_name"),
  variable.name = "date",
  value.name = "home_price"
  )

vals[, `:=`(
  date_string = date, 
  date = as.Date(date, format = "%Y_%m_%d"),
  home_price_80 = home_price * 0.8
  )]

interest[, mortgage_rate := mortgage_30_us / 100]

calculate_total_cost_plus_interest <- function(home_price, mortgage_rate) {
  total_cost_plus_interest <- (home_price * (1+mortgage_rate)^360 - home_price)
  return(total_cost_plus_interest)
}


calculate_monthly_payment <- function(principal, interest_rate, loan_term = 30) {
  #adjust for monthly payments
  interest_rate <- interest_rate/12
  loan_term <- loan_term * 12
  
  M = principal * (interest_rate * (1 + interest_rate) ^ (loan_term)) /
    ((1+interest_rate)^ (loan_term) - 1)
  
  return(M)
}


calculate_total_cost <- function(principal, interest_rate, loan_term = 30) {
  return(calculate_monthly_payment(principal, interest_rate, loan_term) * 
      loan_term * 12)
}

calculate_monthly_payment(400000, 0.04, 30)
calculate_total_cost(400000, 0.04, 30)


val_int <- vals[interest, roll = "nearest", nomatch = 0L, on = "date"]
val_int <- val_int[, `:=`(
  total_cost_30 = calculate_total_cost(home_price_80, mortgage_rate)
  )]
val_int <- melt(
  val_int, 
  measure.vars = c("home_price", "total_cost_30"),
  variable.name = "cost_type",
  value.name = "cost"
  )
val_int[, cost_factor := factor(cost_type, labels = c(
  "Typical U.S. Home Price", 
  "Price + 30-year mortgage"))]

# expect_equal(0, anyDuplicated(val_int[, c('region_id', 'date')]))

val_int_us <- val_int[region_id == "102001" & date >= "2015-01-01"]

plot <- ggplot(val_int_us) +
  # Add cost lines
  geom_line(aes(x = date, y = cost, color = cost_factor), size = 1.1) +
  # Add interest rate line
  geom_line(aes(x = date, y = mortgage_rate * 1e7, linetype = "Interest Rate"), color = "black") +
  scale_linetype_manual(values = c("Interest Rate" = "dashed"))+
  scale_y_continuous(
    labels = scales::dollar,
    # Add secondary axis
    sec.axis = sec_axis(~. / 1e7, 
      name = "Interest Rate", 
      labels = scales::percent)
  ) +
  theme_clean() +
  scale_color_wsj() +
  guides(color = FALSE) +
  theme(
    plot.subtitle = element_markdown(),
    legend.position = c(0.5, 1.01),
    legend.background = element_blank()
  ) +
  labs(
    title = "What housing market crash?",
    subtitle = glue(
      "<span style = 'color:{wsj_pal()(2)[1]}'>Typical home price</span> ",
      "vs <span style = 'color:{wsj_pal()(2)[2]}'>total cost*</span> ",
      "since 2015"),
    x = "",
    y = "",
    color = "",
    linetype = "",
    caption = glue(
      "* Total cost includes a 30-year mortgage with interest after a 20% down payment.",
      "Source: FRED, Zillow, @arthurgailes",
      .sep = "\n")
  )

print(plot)

ggsave("img/housing_price_vs_cost.png", plot)
