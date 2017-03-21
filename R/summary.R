#' @import dplyr
#' @export
summary.budget <- function(object, ...) {
  if (!is.budget(object)) stop("Object is not a budget")

  tbls <- object %>%
    group_by(type, category) %>%
    do(item = tibble(name = .$name,
                     month = .$month,
                     amount = .$amount))

  out <- list(tbls = tbls)
  class(out) <- "summary.budget"
  out
}


#' @export
print.summary.budget <- function(x, ...) {
  cat("Aah, a fine budget.\n")
}