#' Create new budget
#'
#' @return An empty budget data frame.
#'
#' @examples bgt <- budget()
#'
#' @import dplyr
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


#' @rdname budget
#' @export
is.budget <- function(x) {
  classes <- c("budget", "tbl_df", "tbl", "data.frame")
  if (!(inherits(x, classes))) return(FALSE)
  if (dim(x)[2] != 6) return(FALSE)
  cols <- c("name", "type", "category", "owner", "month", "amount")
  if (any(names(x) != cols)) return(FALSE)
  return(TRUE)
}
