#' Boostrap plot for documenting purpose
#'
#' @param df
#' @param sector_name
#' @param building_name
#' @param extra_suffix
#' @param n_bootstrap_samples
#' @param conf
#'
#' @return
#' @export
#'
#' @examples
doc_bootstrap_plot = function(df = data_niche,
                              sector_name,
                              building_name = NULL,
                              extra_suffix = "",
                              n_bootstrap_samples = 500,
                              conf=0.05) {


  if (is.null(building_name)) {
    df <- df %>% filter(sector == sector_name)
  } else {
    df <- df %>% filter(sector == sector_name) %>%
      filter(build %in% building_name)
  }

  set = df %>% get_size
  results = calculate_quantogram(set)
  # TODO: if >1 maximum
  best_score = results %>% group_by %>% filter(f_q == max(f_q))
  original_quanta = data.frame(sector = sector_name,
                               q_hat = best_score$q,
                               peak_hat = best_score$f_q)

  # Bootstrap confidence interval procedure

  results_all = NULL
  print(sector_name)
  p <- progress_estimated(n_bootstrap_samples, min_time = 0)
  results_sector = NULL
  for (n in 1:n_bootstrap_samples) {

    # 1. Sample from original dataset
    sample = sample_bootstrap(set)

    # 2. Estimate quantum and peak
    results = calculate_quantogram(sample)

    # 3. Collect results
    # TODO: if >1 maximum
    win_score = results %>% group_by %>% filter(f_q == max(f_q))
    q_hat = win_score$q
    peak_hat = win_score$f_q

    results_sector[[n]] = data.frame(q_hat, peak_hat)

    p$tick()$print()
  }
  results_sector = do.call(rbind.data.frame, results_sector)
  df_results_all = data.frame(sector = sector_name, results_sector)

  # Plot adjustment
  borders = quantile(df_results_all$q_hat, c(0+conf/2, 1-conf/2))
  df_results_all %>% group_by() %>% summarise(min = min(q_hat), max = max(q_hat)) -> limits
  eps = (limits$max - limits$min) * 1.5 / 2            # scale up x axis range
  center = limits$min + (limits$max - limits$min) / 2  # get center position

  # Plot boostrap confidence interval for quantum
  p <- ggplot(df_results_all,
              aes(q_hat)) +
    geom_histogram(binwidth = 0.005,
                   fill = "white",
                   color = "black") +
    scale_x_continuous(limits = c(pmax(center - eps, 0), pmin(center + eps, 1))) +
    geom_vline(
      data = original_quanta,
      aes(xintercept = q_hat),
      colour = col_blu,
      linetype = "dashed",
      size = 1) +
    geom_vline(
      xintercept = borders,
      colour = col_neg,
      linetype = "dashed",
      size = 1) +
    xlab(label = "quantum estimation") +
    theme_minimal()
  p
}
