#' Get KDE distribution
#'
#' @param data_input
#' @param band
#'
#' @return
#' @export
#'
#' @examples
calculate_KDE = function(data_input, band = 0.05) {
  bkde(data_input, kernel = "normal", bandwidth = band) %>%
    as.data.frame() %>%
    filter(x > 0 & y > 0) -> kde
  return(kde)
}

#' Sample from KDE distribution
#'
#' @param kde
#' @param size
#'
#' @return
#' @export
#'
#' @examples
sample_KDE = function(kde, size = 1000) {
  sample <- sample(size = size, x = kde$x, replace = T, prob = kde$y/sum(kde$y))
  return(sample)
}
