#' Plot cosine quantogram
#'
#' @param df_results
#' @param sector_label
#'
#' @return
#' @export
#'
#' @examples
plot_quantogram = function(df_results, sector_label) {

  names(df_results) <- c("q", "quantogram.score")
  quantum <- df_results %>%
    group_by %>%
    filter(quantogram.score==max(quantogram.score)) %>% .$q

  if (quantum %>% length > 1) {
    message("Recognised multiple candidates for quantum. The highest value was selected")
    message(paste0(quantum, sep=" "))
    quantum = max(quantum)
  }

  title = sector_label
  subtitle = paste0("Peak in quantum = ", quantum)

  sp <- ggplot(df_results, aes(x=q, y=quantogram.score)) +
    geom_line() +
    geom_point()
  sp <- sp +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_vline(xintercept=quantum, colour="#BB0000", linetype="dashed") +
    theme_minimal() +
    ggtitle(label = title,
            subtitle = subtitle)
  return(sp)
}
