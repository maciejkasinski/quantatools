#' Calculate Kendall's cosine quantogram
#'
#' @param x A numeric vector of measurement values from which
#' the quantogram should be calculated
#' @param params cosine quantogram parameters
#' including "RNG_START", "RNG_END", "STEP", "Q_MIN"
#'
#' @return a data frame with quanta and f(q)
#' @export
#'
#' @examples
calculate_quantogram <- function(x,
                                params = getOption("CONSTANTS_QUANTOGRAM")) {

  if (4 != sum(c("RNG_START", "RNG_END", "STEP", "Q_MIN") %in% names(params))) {
    print("Incorrect number of parameters")
    return(NULL)
  }

  A <- sqrt(2 / length(x))

  rng <- seq(params$RNG_START, params$RNG_END, by = params$STEP)
  results <- as.data.frame(matrix(ncol = 2, nrow = length(rng)))
  colnames(results) <- c("q", "f_q")
  results$q <- rng

  results$f_q <- vapply(results$q,
                        FUN.VALUE = numeric(1),
                        FUN = function(y) get_fofq(try_q = y,
                                                   x = x,
                                                   params = params,
                                                   A = A))
  return(results)
}


#' Calculate f(q)
#'
#' @param try_q
#' @param params
#' @param x
#'
#' @return
#' @export
#'
#' @examples
get_fofq <- function(try_q, params, x, A) {
  q <- try_q
  if (q < params$Q_MIN) {
    f_q <- 0
  } else {
    sum = 0
    e <- x %% q
    cosVal <- 2.0 * pi * e / q
    cosVal <- cos(cosVal)
    sum <- sum(cosVal)
    f_q = A * sum
  }
  return(f_q)
}


#' Simply calculate a quantum
#'
#' @param x A numeric vector of measurement values from which
#' the quantogram should be calculated
#'
#' @return The quantum of x
#' @export
#'
#' @examples
get_quantum = function(x) {
  quantogram <- calculate_quantogram(x)
  quantum <- quantogram$q[which.max(quantogram$f_q)]
  return(quantum)
}
