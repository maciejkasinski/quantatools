#' Plot and calculate SLS results
#'
#' @param x numeric vector of measurement values
#' @param sector_name
#' @param params
#'
#' @return
#' @export
#'
#' @examples
plot_calculate_selective_least_squares = function(x, sector_name, params=getOption("CONSTANTS_SLS")) {

  all_quanta <- seq(params$SLS_RNG_START, params$SLS_RNG_END, params$SLS_STEP)
  quanta_pairs <- combn(all_quanta, params$SLS_MAX_N_QUANTA)

  scores = NULL
  pb <- progress_estimated(ncol(quanta_pairs), min_time = 0)
  for (i in 1:ncol(quanta_pairs)) {
    quantum_1 <- quanta_pairs[1, i]
    quantum_2 <- quanta_pairs[2, i]
    score <- calculate_selective_least_squares(x = x, quanta = c(quantum_1, quantum_2))
    scores <- rbind(scores, data.frame(quantum_1 = quantum_1, quantum_2 = quantum_2, score = score))
    pb$tick()$print()
  }

  p <- ggplot(scores %>% mutate(new_score = 1 / score ^ 2),
              aes(x = quantum_1,
                  y = quantum_2)) +
    geom_point(aes(col = new_score, size = new_score)) +
    guides(colour = "none", size = "none") +
    ggtitle(label = sector_name) +
    theme_minimal()
  return(list("p"=p, "scores"=scores))
}
