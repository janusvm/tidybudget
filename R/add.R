#' Add new items to a budget.
#'
#' Main functions for adding income channels and expenses to a budget.
#'
#' @param budget Budget object to add to.
#' @param name Name of the item.
#' @param amount Money amount.
#'
#'   Can be either a single numeric, or a vector.
#'   If vector, its length should evenly divide that of `months`.
#' @param category Category of income or expense.
#'
#'   For example `"housing"`, `"savings"`, etc.
#' @param owner Name of the person whose item this is.
#'
#'   Used to generate individual budgets from joint budgets.
#' @param months Vector of integers corresponding to the months in which the
#'   income/expense is active.
#'
#' @examples
#' mybudget <- budget() %>%
#'   add_income("su", 5300, "grants", "Janus") %>%
#'   add_expense("rent", 3700, "housing", "Janus")
#'
#' @name add
NULL

#' @export
#' @rdname add
add_income <- function(budget, name, amount, category, owner, months = 1:12) {

  new_item <- tibble(name = name,
                     type = "income",
                     category = category,
                     owner = owner,
                     month = months,
                     amount = amount)

  return(bind_rows(budget, new_item))
}

#' @export
#' @rdname add
add_expense <- function(budget, name, amount, category, owner, months = 1:12) {

  new_item <- tibble(name = name,
                     type = "expense",
                     category = category,
                     owner = owner,
                     month = months,
                     amount = amount)

  return(bind_rows(budget, new_item))
}
