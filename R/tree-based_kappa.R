#' Make a jitter plot
#'
#' Plot a numerical predictor agains a binary target.
#'
#' @param x A numerical vector.
#' @param y A binary variable that can be coerced to factor.
#'
#' @return An object of class ggplot.
#' @seealso \url{https://edwinth.github.io/blog/tree-based-kappa/}
#' @export

predictor_plot <- function(x, y) {
  stopifnot(x %>% is.numeric)
  stopifnot(y %>% unique %>% length %>% `==`(2))
  data_frame(x = x, y = as.factor(y), y_axis = 0) %>%
    ggplot(aes(x, y_axis)) +
    geom_jitter(aes(col = y)) +
    ylab('') +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

#' Make a gini plot
#'
#' Plots the ROC curve and also displays the univariate gini of \code{x}.
#'
#' @param x A numerical vector.
#' @param y A binary variable that can be coerced to factor.
#'
#' @return An object of class ggplot.
#' @seealso \url{https://edwinth.github.io/blog/tree-based-kappa/}
#' @export

gggini <- function(x, y) {
  roc_obj <- pROC::roc(y, x)
  roc_plot <- data_frame(FPR = roc_obj$sensitivities,
                         FNR = 1 - roc_obj$specificities) %>%
    group_by(FNR) %>% summarise(FPR = max(FPR)) %>%
    rbind(data_frame(FPR = 0, FNR = 0))
  gini <- round(2 * (roc_obj$auc - 0.5), 2)

  ggplot(roc_plot, aes(FNR, FPR)) +
    geom_line() +
    geom_segment(aes(0, 0, xend = 0, yend = 1), col = "black", lty = 2) +
    geom_segment(aes(0, 1, xend = 1, yend = 1), col = "black", lty = 2) +
    geom_segment(aes(0, 0, xend = 1, yend = 0), col = "black", lty = 2) +
    geom_segment(aes(1, 0, xend = 1, yend = 1), col = "black", lty = 2) +
    geom_segment(aes(0, 0, xend = 1, yend = 1), col = "red") +
    annotate("text", x = 0.8, y = 0.2, label = sprintf("Gini = %s", gini))
}

#' Get the tree-based kappa.
#'
#' Split x and y into k folds. For each fold; train a univariate tree and get
#' the probalities for the hold-out. Then, obtain the optimal split and calculate
#' Cohen's kappa on y and y hat.
#'
#' @param x A numerical vector.
#' @param y A binary variable that can be coerced to factor.
#' @param k The number of folds to use in cross-validation.
#'
#' @return An object of class ggplot.
#' @seealso \url{https://edwinth.github.io/blog/tree-based-kappa/}
#' @export
get_kappa <- function(x, y, k = 10) {
  stopifnot(length(unique(y)) == 2)
  stopifnot(length(y) == length(x))

  model_set <- data_frame(y = y,
                          x = x,
                          cv_grp = add_crosval_groups(length(y), k))

  scored_set <- lapply(1:k, train_one_split, model_set = model_set) %>%
    do.call("rbind", .) %>% as_data_frame

  best_split <- determine_split(scored_set$`1`, scored_set$y)
  y_hat_cat <- as.numeric(scored_set$`1` > best_split)

  kappa <- calc_kappa(y_hat_cat, scored_set$y)
  return(round(kappa, 3))
}

add_crosval_groups <- function(n, k) {
  groups <- rep(1:k, n %/% k)
  if ((n %% k) != 0) groups <- groups %>% c(1:(n %% k))
  sample(groups)
}

train_one_split <- function(model_set, k) {
  train <- model_set %>% filter(cv_grp != k)
  hold_out  <- model_set %>% filter(cv_grp == k)
  mod   <- rpart::rpart(y ~ x, data = train, method = "class")
  yhat  <- rpart::predict.rpart(mod, hold_out)
  cbind(y = hold_out$y, yhat)
}

calc_kappa <- function(predicted, observed) {
  ct <- table(observed, predicted)
  if (!isTRUE(all.equal(dim(ct), c(2, 2)))) {
    return(NA)
  }
  n <- sum(ct)
  p_0 <- sum(diag(ct)) / n
  m_y <- sum(ct[,1]) * sum(ct[1,]) / n
  m_yhat <- sum(ct[,2]) * sum(ct[2,]) / n
  p_e <- (m_y + m_yhat) / n
  (p_0 - p_e)  / (1 - p_e)
}

determine_split <- function(y_hat_num,
                            y,
                            splits = seq(0.01, 0.99, by = .01)){
  y_hat_list <- map(splits, function(s) as.numeric (y_hat_num > s))
  kappas     <- map_dbl(y_hat_list, calc_kappa, observed = y)
  max_splits <- which(kappas == max(kappas, na.rm = TRUE))
  return_split <- max_splits[ ceiling(length(max_splits) / 2) ]
  return(splits[return_split])
}
