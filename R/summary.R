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


#' @import dplyr
#' @export
mean.budget <- function(x, by = c("type", "category"), ...) {
  group_var <- match.arg(by)
  bgt_means <- x %>%
    group_by_(group_var) %>%
    summarise_at("amount", funs(sum(., na.rm = TRUE)/12)) %>%
    ungroup
  return(bgt_means)
}
