pacman::p_load(data.table, here, snakecase, testthat, ggplot2)
setwd(here())

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

source('code/function_interest.R')

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
  "Nominal Home Price",
  "Total cost of a 30-year mortage \nafter a 20% down payment \nfactoring in current interest rates"))]

# expect_equal(0, anyDuplicated(val_int[, c('region_id', 'date')]))

val_int_us <- val_int[region_id == "102001" & date >= "2010-01-01"]

ggplot(val_int_us, aes(x = date, cost, color = cost_factor)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  labs(color = NULL)
