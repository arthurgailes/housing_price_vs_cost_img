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