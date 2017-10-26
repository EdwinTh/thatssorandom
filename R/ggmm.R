#' Create a Marimekko/Mosaic plot with ggplot2
#' 
#' Take two categorical variables and create a Marimekko/Mosaic plot from it.
#' @param df A data frame containing \code{x} and \code{y}.
#' @param x The bare name of the variable for the x-axis.
#' @param y The bare name of the variable for the y-axis and the fill.
#' @param alpha_condition All cells meeting this condition will have the alpha
#' set to 1. Cells not meeting the condition have the alpha set to 0.4.
#' @return An object of class \code{ggplot2}.
#' @details The \code{alpha_condition} enables to highlight specific parts of 
#' the plot. This is especially helpful for highlighting marginal distributions.
#' @examples 
#' ggmm(mtcars, cyl, vs)
#' ggmm(mtcars, cyl, vs, alpha_condition = vs == 1)
#' ggmm(mtcars, cyl, vs, alpha_condition = cyl == 6)

ggmm <- function(df, x, y, alpha_condition = 1 == 1) {
  library(tidyverse)
  x_q <- enquo(x)
  y_q <- enquo(y)
  a_q <- enquo(alpha_condition)
  
  plot_set <- df %>% 
    add_alpha_ind(a_q) %>% 
    x_cat_y_cat(x_q, y_q) %>% 
    add_freqs_col()
  
  plot_return <- mm_plot(plot_set)

  set_alpha(df, plot_return, a_q)
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

mm_plot <- function(plot_set) {
  plot_set %>% 
    ggplot(aes(x_cat, freq, width = y_cnt, fill = y_cat, alpha = alpha_ind)) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    facet_grid(~x_cat, scales = "free_x", space = "free_x") +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines")
    ) +
    guides(alpha = FALSE) +
    labs(fill = quo_name(y_q)) +
    xlab(quo_name(x_q)) 
}

set_alpha <- function(df, plot_return, a_q) {
  if (mutate(df, !!a_q) %>% pull() %>% 
      unique() %>% length() %>% magrittr::equals(1)) {
    plot_return +
      scale_alpha_continuous(range = c(1))
  } else {
    plot_return +
      scale_alpha_continuous(range = c(.4, 1))
  }
}
