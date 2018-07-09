#' Get sample error given a quantum
#'
#' @param input numeric vector of measurement values
#' @param quantum numeric value of quantum
#'
#' @export
#'
calculate_error = function(input, quantum) {
  n_multiply = round(input / quantum, 0)
  error = input - n_multiply * quantum
  return(error)
}
