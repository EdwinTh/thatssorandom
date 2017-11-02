#' Create a Marimekko/Mosaic plot with ggplot2
#'
#' Take two categorical variables and create a Marimekko/Mosaic plot from it.
#' @param df Data frame containing \code{x} and \code{y}.
#' @param x Bare name of the variable for the x-axis.
#' @param y Bare name of the variable for the y-axis and the fill.
#' @param alpha_condition All cells meeting this condition will have the alpha
#' set to 1. Cells not meeting the condition have the alpha set to 0.4.
#' @param add_text Add cell count, cell proportion of total, or cell
#' percentage of total.
#' @param round_text Nr of decimals, only applies to proportions and percentages.
#' @return An object of class \code{ggplot2}.
#' @details The \code{alpha_condition} enables to highlight specific parts of
#' the plot. This is especially helpful for highlighting marginal distributions.
#' @examples
#' ggmm(mtcars, cyl, vs)
#' ggmm(mtcars, cyl, vs, alpha_condition = vs == 1)
#' ggmm(mtcars, cyl, vs, alpha_condition = cyl == 6)
#' @export
ggmm <- function(df,
                 x,
                 y,
                 alpha_condition = 1 == 1,
                 add_text        = c(NA, "n", "prop", "perc"),
                 round_text      = 2) {
  stopifnot(is.data.frame(df))
  add_text <- match.arg(add_text)

  x_q <- enquo(x)
  y_q <- enquo(y)
  a_q <- enquo(alpha_condition)

  plot_set <- df %>%
    add_alpha_ind(a_q) %>%
    x_cat_y_cat(x_q, y_q) %>%
    add_freqs_col()

  plot_return <- mm_plot(plot_set, x_q, y_q)

  plot_return <- set_alpha(df, plot_return, a_q)

  if (!is.na(add_text)) {
    plot_set$text <- make_text_vec(plot_set, add_text, round_text)
    plot_set$freq <- calculate_coordinates(plot_return)
    text_part <- geom_text(data = plot_set, aes(label = text))
  } else {
     text_part <- NULL
  }

  plot_return + text_part
}

add_alpha_ind <- function(df, a_q) {
  df %>%
    mutate(alpha_ind = !!a_q)
}

x_cat_y_cat <- function(df, x_q, y_q) {
  df %>%
    mutate(x_cat = as.character(!!x_q),
                  y_cat = as.character(!!y_q))
}

add_freqs_col <- function(df) {
  stopifnot(all(c('x_cat', 'y_cat', 'alpha_ind') %in% colnames(df)))
  df %>%
    group_by(x_cat, y_cat) %>%
    summarise(comb_cnt  = n(),
              alpha_ind = as.numeric(sum(alpha_ind) > 0)) %>%
    mutate(freq  = comb_cnt /sum(comb_cnt),
           y_cnt = sum(comb_cnt)) %>%
    ungroup()
}

mm_plot <- function(plot_set, x_q, y_q) {
  plot_set %>%
    ggplot(aes(x_cat, freq, width = y_cnt, fill = y_cat, alpha = alpha_ind)) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    facet_grid(~x_cat, scales = "free_x", space = "free_x",
               switch = "x") +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank()
    ) +
    guides(alpha = FALSE) +
    labs(fill = quo_name(y_q)) +
    xlab(quo_name(x_q))
}

set_alpha <- function(df, plot_return, a_q) {
  if (mutate(df, !!a_q) %>% pull() %>%
      unique() %>% length() %>% `==`(1)) {
    plot_return +
      scale_alpha_continuous(range = c(1))
  } else {
    plot_return +
      scale_alpha_continuous(range = c(.4, 1))
  }
}

make_text_vec <- function(plot_set, add_text, round_text) {
  if (add_text == "n") return(get_counts(plot_set))
  text_col <- get_props(plot_set)
  if (add_text == "perc") {
    text_col <- round(text_col * 100, round_text)
    return(paste0(text_col, "%"))
  }
  round(text_col, round_text)
}

get_counts <- function(plot_set) {
  plot_set %>% pull(comb_cnt)
}

get_props <- function(plot_set){
  plot_set %>%
    mutate(text_col = comb_cnt / sum(plot_set$comb_cnt)) %>%
    pull()
}

calculate_coordinates <- function(plot_return) {
  ggplot_build(plot_return)$data[[1]] %>%
    split(.$PANEL) %>%
    map(y_in_the_middle) %>%
    unlist()
}

y_in_the_middle <- function(x) {
  y_pos <- c(0, x$y)
  rev(y_pos[-length(y_pos)] + (y_pos %>% diff()) / 2)
}
