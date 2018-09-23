#' Plot measurements in order for documenting purpose
#'
#'
#' @param df
#' @param sector_name
#' @param building_name
#' @param margin allowed deviation from single quantum (as percentage of quantum). An interval from `(1 - margin)*quantum` to `(1 + margin)*quantum` is plotted.
#' @param color
#'
#' @return
#' @export
#'
#' @examples
doc_size_distr_plot = function(df = data_niche,
                               sector_name,
                               building_name = NULL,
                               margin = 0.1,
                               color = "black") {

  annotate_rect = function(df_rng, times) {
    annotate(
      geom = "rect",
      xmin = df_rng$min,
      xmax = df_rng$max,
      ymin = q0 * times * (1 - margin),
      ymax = q0 * times * (1 + margin),
      alpha = 0.3,
      fill = "blue"
    )
  }

  if (is.null(building_name)) {
    df <- df %>% filter(sector == sector_name)
  } else {
    df <- df %>% filter(sector == sector_name) %>%
      filter(build %in% building_name)
  }

  quantogram = calculate_quantogram(df %>% .$size %>% unround_data)
  q0 = quantogram %>% group_by() %>% filter(f_q == max(f_q)) %>% .$q

  df_plot <- filter(df, sector == sector_name) %>%
    arrange(size) %>%
    mutate(x = row_number())

  df_plot %>%
    mutate(
      close_to_q1 = ifelse(abs(q0 - size) / (q0) <= margin, T, F),
      close_to_q2 = ifelse(abs(q0 * 2 - size) / (q0 * 2) <= margin, T, F),
      close_to_q3 = ifelse(abs(q0 * 3 - size) / (q0 * 3) <= margin, T, F)
    ) -> df2


  df2 %>% group_by(close_to_q1) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q1) -> rng1
  df2 %>% group_by(close_to_q2) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q2) -> rng2
  df2 %>% group_by(close_to_q3) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q3) -> rng3

  # if not a single size within 1*quantum margin error
  if (nrow(rng1) == 0) {
    x_rng1 = data.frame()
    x_rng1[1, "min"] <- 0
    x_rng1[1, "max"] <- 0
  } else {
    filter(df_plot, size >= rng1$min, size <= rng1$max) %>%
      summarise(min = min(x), max = max(x)) -> x_rng1
  }

  # if not a single size within 2*quantum margin error
  if (nrow(rng2) == 0) {
    x_rng2 = data.frame()
    x_rng2[1, "min"] <- 0
    x_rng2[1, "max"] <- 0
  } else {
    filter(df_plot, size >= rng2$min, size <= rng2$max) %>%
      summarise(min = min(x), max = max(x)) -> x_rng2
  }

  # if not a single size within 3*quantum margin error
  if (nrow(rng3) == 0) {
    x_rng3 = data.frame()
    x_rng3[1, "min"] <- 0
    x_rng3[1, "max"] <- 0
  } else {
    filter(df_plot, size >= rng3$min, size <= rng3$max) %>%
      summarise(min = min(x), max = max(x)) -> x_rng3
  }

  p = ggplot(df_plot, aes(x = x, y = size)) +
    geom_segment(aes(xend = x, yend = 0), size = 0.3, color=color) +
    scale_x_continuous(name = "num of measurements", labels = NULL) +
    scale_y_continuous(name = "length [cm]", limits = c(0, 1.5)) +
    geom_dl(aes(label = sector_name),
            method = list("last.points",
                          rot = 0,
                          hjust = 1.1,
                          vjust = 0.5)) +
    guides(colour = "none") +
    theme_minimal()

  p +
    annotate_rect(x_rng1, 1) +
    annotate_rect(x_rng2, 2) +
    annotate_rect(x_rng3, 3)
}
