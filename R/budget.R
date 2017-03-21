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

#' @importFrom dplyr group_by summarise_at ungroup
#' @importFrom magrittr "%>%"
#'
#' @export
mean.budget <- function(x, by = c("type", "category"), ...) {
  group_var <- match.arg(by)
  bgt_means <- x %>%
    group_by_(group_var) %>%
    summarise_at("amount", funs(sum(., na.rm = TRUE)/12)) %>%
    ungroup
  return(bgt_means)
}
