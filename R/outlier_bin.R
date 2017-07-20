#' Create a Histogram with Outlier Bins
#'
#' Make a ggplot with `geom_histogram` that contains outlier bins. Bins can
#' be created for both floor and ceiling outliers. Code is experimental
#' so expect some deviations from normal plotting, like messed-up grid lines.
#' @param x a dataframe.
#' @param var_name the name of the variable to visualize.
#' @param cut_off_ceiling the ceiling of the histogram, all values above it
#' will be put in the same bin. If `NA`` no ceiling bin is created.
#' @param cut_off_floor the floor of the histogram, all values below it
#' will be put in the same bin. If `NA`` no floor bin is created.
#' @param col the color of the histogram.
#' @param fill the fill of the regular histogram bins.
#' @param fill_outlier_bins the fill of the outlier bins, make them a separate
#' color to stress there different nature.
#' @param binwidth the binwidth of the histogram.
#' @seealso \url{https://edwinth.github.io/blog/outlier-bins/}
#' @examples
#' library(tidyverse)
#' data_frame(x = c(runif(100, 0, 1000), rnorm(1000, 500, 3))) %>%
#'  gg_outlier_bin("x", 520, 480)
#' @export

gg_outlier_bin <- function(x,
                           var_name,
                           cut_off_floor,
                           cut_off_ceiling,
                           col = "black",
                           fill = "cornflowerblue",
                           fill_outlier_bins = "forestgreen",
                           binwidth = NULL) {

  printing_min_max <- x %>% summarise_(sprintf("round(min(%s, na.rm = TRUE), 1)", var_name),
                                       sprintf("round(max(%s, na.rm = TRUE), 1)", var_name))

  ceiling_filter <- ifelse(!is.na(cut_off_ceiling),
                           sprintf("%s < %f", var_name, cut_off_ceiling),
                           "1 == 1")
  floor_filter   <- ifelse(!is.na(cut_off_floor),
                           sprintf("%s > %f", var_name, cut_off_floor),
                           "1 == 1")

  x_regular <- x %>% filter_(ceiling_filter, floor_filter) %>%
    select_(var_name)

  x_to_roll_ceiling <- x %>% filter_(
    sprintf("%s >= %f", var_name, cut_off_ceiling)) %>% select_(var_name)
  if (!is.na(cut_off_ceiling)) x_to_roll_ceiling[, 1] <- cut_off_ceiling

  x_to_roll_floor <- x %>% filter_(
    sprintf("%s <= %f", var_name, cut_off_floor)) %>% select_(var_name)
  if (!is.na(cut_off_floor)) x_to_roll_floor[, 1] <- cut_off_floor

  plot_obj <- ggplot(x_regular, aes_string(var_name)) +
    geom_histogram(col = col, fill = fill, binwidth = binwidth)

  if (!is.na(cut_off_ceiling)) {
    ticks_for_ceiling <- update_tickmarks_ceiling(plot_obj, cut_off_ceiling,
                                                  printing_min_max[1,2])
    plot_obj <- plot_obj +
      geom_histogram(data = x_to_roll_ceiling, fill = fill_outlier_bins, col = col,
                     binwidth = binwidth) +
      scale_x_continuous(breaks = ticks_for_ceiling$tick_positions,
                         labels = ticks_for_ceiling$tick_labels)
  }

  if (!is.na(cut_off_floor)) {
    ticks_for_floor <- update_tickmarks_floor(plot_obj, cut_off_floor,
                                              printing_min_max[1,1])
    plot_obj <- plot_obj +
      geom_histogram(data = x_to_roll_floor, fill = fill_outlier_bins,
                     col = col, binwidth = binwidth) +
      scale_x_continuous(breaks = ticks_for_floor$tick_positions,
                         labels = ticks_for_floor$tick_labels)
  }

  return(plot_obj)
}


update_tickmarks_ceiling <- function(gg_obj,
                                     co,
                                     max_print) {
  ranges <- suppressMessages(
    ggplot_build(gg_obj)$layout$panel_ranges[[1]])
  label_to_add <- sprintf("(%s , %s)", round(co, 1), max_print)
  tick_positions <- ranges$x.major_source
  tick_labels    <- ranges$x.labels
  if (overlap_ceiling(tick_positions, co)) {
    tick_positions <- tick_positions[-length(tick_positions)]
    tick_labels    <- tick_labels[-length(tick_labels)]
  }
  return(list(tick_positions = c(tick_positions, co),
              tick_labels    = c(tick_labels, label_to_add)))
}

overlap_ceiling <- function(positions, cut_off) {
  n <- length(positions)
  ticks_dif <- positions[n] - positions[n-1]
  (cut_off - positions[n]) / ticks_dif < 0.25
}

update_tickmarks_floor <- function(gg_obj,
                                   co,
                                   min_print) {
  ranges <- suppressMessages(
    ggplot_build(gg_obj)$layout$panel_ranges[[1]])
  label_to_add <- sprintf("(%s , %s)", min_print, round(co, 1))
  tick_positions <- ranges$x.major_source
  tick_labels    <- ranges$x.labels
  if (overlap_floor(tick_positions, co)) {
    tick_positions <- tick_positions[-1]
    tick_labels    <- tick_labels[-1]
  }
  return(list(tick_positions = c(co, tick_positions),
              tick_labels    = c(label_to_add, tick_labels)))
}

overlap_floor <- function(positions, cut_off) {
  ticks_dif <- positions[2] - positions[1]
  (positions[1] - cut_off) / ticks_dif < 0.25
}
