#' Calculate selective least squares method (by Paakanen)
#'
#' @description
#'
#' @param x numeric vector of measurement values
#' @param quanta
#'
#' @return
#' @export
#'
#' @examples
calculate_selective_least_squares = function(x, quanta) {

  least_absolute_remainder = function(z, q) {
    lower <- floor(z / q)
    upper <- ceiling(z / q)

    rem_lower <- z - q * lower
    rem_upper <- z - q * upper

    abs_rem <- pmin(abs(rem_lower), abs(rem_upper))
    min_abs_rem <-
      ifelse(abs_rem == abs(rem_upper), rem_upper, rem_lower)
    min_abs_rem
  }

  quanta <- unique(quanta)
  sum = 0
  for (i in 1:length(x)) {
    X <- x[i]
    g_sq = c()
    for (j in 1:length(quanta)) {
      g_sq <- c(g_sq, least_absolute_remainder(X, quanta[j]) ^ 2)
    }
    sum <- sum + min(g_sq)
  }
  sum
}
