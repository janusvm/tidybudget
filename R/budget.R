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
  class(bgt) <- append(class(bgt), "budget")
  return(bgt)
}