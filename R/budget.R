#' Create new budget
#'
#' @return An empty budget data frame.
#'
#' @examples bgt <- budget()
#'
#' @export
budget <- function() {
  bgt <- tibble(name = character(),
                type = character(),
                category = character(),
                owner = character(),
                month = integer(),
                amount = double())
  class(bgt) <- c("budget", class(bgt))
  return(bgt)
}


#' @export
mean.budget <- function(x, by = c("type", "category"), ...) {
  group_var <- match.arg(by)
  bgt_means <- x %>%
    group_by_(group_var) %>%
    summarise_at("amount", funs(sum(., na.rm = TRUE)/12)) %>%
    ungroup
  return(bgt_means)
}

#' @export
print.budget <- function(x) {
  counts <- x %>% group_by(type) %>% summarise(count = n_distinct(name))
  n_income <- counts %>% filter(type == "income") %>% .[["count"]]
  n_expense <- counts %>% filter(type == "expense") %>% .[["count"]]

  cat("Budget of", n_income, "incomes and", n_expense, "expenses.\n")
  cat("\n")

  totals <- x %>% group_by(type) %>% summarise(total = sum(amount))
  total_income <- totals %>% filter(type == "income") %>% .[["total"]]
  total_expenses <- totals %>% filter(type == "expense") %>% .[["total"]]

  cat("Total annual income:", total_income, "\n")
  cat("Total annual expenses:", total_expenses, "\n")
  cat("Balance:", total_income - total_expenses, "\n")
  cat("\n")

  income_cats <- x %>% filter(type == "income") %>% .[["category"]] %>% unique
  expense_cats <- x %>% filter(type == "expense") %>% .[["category"]] %>% unique

  cat("Income categories:", paste(income_cats, collapse = ", "), "\n")
  cat("Expense categories:", paste(expense_cats, collapse = ", "), "\n")

}