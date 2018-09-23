#' Get bootstrap sample
#'
#' @param x
#' @param decrease_by
#'
#' @return
#' @export
#'
#' @examples
sample_bootstrap = function(x, decrease_by=0) {
  sample(size = length(x) - decrease_by,
         x = x,
         replace = T)
}
