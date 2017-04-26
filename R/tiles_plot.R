# helper function that checks for the next tile to be sampled if there
# are any colors that should be excluded because the max adjacent was
# reached either vertically or horizontally

check_colors <- function(plot_data,
                         cur_width,
                         cur_height,
                         m_a){
  if (cur_height > m_a){
    colors_height <-
      plot_data[Height %in% (cur_height - (m_a)):(cur_height - 1) &
                  Width == cur_width, color] %>% unique
  } else {

    colors_height <- NULL
  }

  if (cur_width > m_a){
    colors_width <-
      plot_data[Width %in% (cur_width - (m_a)):(cur_width - 1) &
                  Height == cur_height, color] %>% unique
  } else {
    colors_width <- NULL
  }

  if (length(colors_height) > 1) colors_height <- NULL
  if (length(colors_width) > 1) colors_width <- NULL
  exclude <- c(colors_height, colors_width)
  if (length(exclude) == 0) exclude <- 0

  return(exclude)
}

# helper function that samples a tile color from a vector with remaining tiles
# excluding a color if necesarry. Returns the sample color and the vector with
# remaining colors for the next iteration.
sample_color <- function(exclude = 0,
                         cts){
  if (cts %>% is_in(exclude) %>% all){
    stop("There is no valid solution due to adjacency constraint,
         please try again")
  }

  valid_color <- FALSE
  while (valid_color == FALSE){
    color <- sample(cts, 1)
    if (color %>% is_in(exclude) %>% not) {
      valid_color <- TRUE
      return_list <- list(color = color,
                          cts =
                            cts[ - ( (cts == color) %>% which.max)] )
    }
  }
  return(return_list)
}

#' Make a Random Bathroom Design
#'
#' Function will generate a random pattern of tiles that can serve
#' as a bathroom design.
#' @param colors character vector with the colors to use.
#' @param nr_of_each_col prevalence of each color in \code{colors}, should
#' have same length as \code{colors}.
#' @param nr_height nr of tiles in vertical direction.
#' @param nr_width nr of tiles in horizontal directions.
#' @param max_adjacent maximum nr of adjacent tiles of the same color in
#' both directions.
#' @seealso \url{https://edwinth.github.io/blog/bathroom-with-r/}
#' @return A \code{ggplot2} object with the pattern.
#' @examples tiles_pattern()
#'
#' @export
tiles_pattern <- function(
  colors         = c("darkblue", "cyan3", "lightblue1", "white"),
  nr_of_each_col = rep(40, 4),
  nr_height      = 8,
  nr_width       = 20,
  max_adjacent   = 2) {

  if (length(colors) != length(nr_of_each_col)){
    stop("nr_of_each_col vector should be same length as the colors vector")
  }
  if (sum(nr_of_each_col) != nr_height * nr_width){
    stop("Sum nr_of_each_col should equal nr_height * nr_width")
  }

  plot_data <- expand.grid(1:nr_height, 1:nr_width) %>% as.data.table
  colnames(plot_data) <- c("Height", "Width")
  plot_data$color <- integer(nrow(plot_data))

  colors_to_sample <- rep(1:length(colors), nr_of_each_col)

  for (i in 1:(nr_width)){
    for (j in 1:nr_height){
      exclude_iter     <- check_colors(plot_data, i, j, max_adjacent)
      color_iter       <- sample_color(exclude_iter, colors_to_sample)
      plot_data[Height == j & Width == i, color := color_iter$color]
      colors_to_sample <- color_iter$cts
    }
  }

  plot_data[, color := color %>% as.character]

  # build the plot
  ggplot(plot_data, aes(Width, Height)) +
    geom_tile(aes(fill = color), col = "grey") +
    scale_fill_manual(values = c("1" = colors[1],
                                 "2" = colors[2],
                                 "3" = colors[3],
                                 "4" = colors[4])) +
    xlab("") +
    ylab("") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(
      plot.background = element_blank()
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , panel.border = element_blank()
      , axis.ticks.x = element_blank()
      , axis.ticks.y = element_blank()
      , axis.text.x = element_blank()
      , axis.text.y = element_blank()
    ) +
    coord_fixed()
}
