#' Calculate Kendall's cosine quantogram
#'
#' @param x numeric vector of measurement values
#' @param params cosine quantogram parameters including "RNG_START", "RNG_END", "STEP", "Q_MIN"
#'
#' @export
#'
calculate_quantogram = function(x,
                                params=CONSTANTS_QUANTOGRAM) {

  if (4 != sum(c("RNG_START", "RNG_END", "STEP", "Q_MIN") %in% names(params))) {
    print("Incorrect number of parameters")
    return(NULL)
  }

  n = x %>% length
  A = sqrt(2 / n)
  results = NULL

  rng <- seq(params$RNG_START, params$RNG_END, by = params$STEP)
  for (q in rng) {
    if (q < params$Q_MIN)
      next
    sum = 0
    for (i in 1:n) {
      e = x[i] %% q
      cosVal = 2.0 * pi * e / q
      sum = sum + cos(cosVal)
    }
    f_q = A * sum # quanta
    results = rbind(results, data.frame(q, f_q))
  }
  return(results)
}


#' Simply calculate a quantum
#'
#' @param x
#'
#' @export
#'
get_quantum = function(x) {
  calculate_quantogram(x) %>% filter(f_q==max(f_q)) %>% pull(q)
}
