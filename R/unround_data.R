#' Unrounding method
#'
#' @param input
#' @param depth
#'
#' @return
#' @export
#'
#' @examples
unround_data = function(input, depth=0.01) {
  output = input + round(
    sample(
      c(-depth / 2, depth / 2),
      size = input %>% length,
      replace = T
    ), 3)
  return(output)
}
