#' Functions that form a wrapper around \code{ifelse}.
#'
#' \code{ifelse} functions can be nested to do a vectorised if then else
#' evaluation, with multiple if then statements. Although this is functional it
#' often gives messy code. These three functions provide a wrapper, so the
#' code gets cleaner.
#'
#' @param if_stat Logical condition forming the if statement.
#' @param then Value to be returned, when \code{if_stat} is \code{TRUE}.
#' @param else_ret Value to be returned, when all \code{if_stat}s are
#' \code{FALSE}.
#' @seealso \url{https://edwinth.github.io/blog/ifelse-wrapper/}
#' @examples
#' set.seed(0310)
#' x <- runif(1000, 1, 20)
#' y <- runif(1000, 1, 20)
#' ie(
#'   i(x < 5 & y < 5,   'A'),
#'   i(x < 5 & y < 15,  'B'),
#'   i(x < 5,           'C'),
#'   i(x < 15 & y < 5,  'D'),
#'   i(x < 15 & y < 15, 'E'),
#'   i(y < 5,           'F'),
#'   i(y < 15,          'G'),
#'   e('H')
#' )
#' @export
i <- function(if_stat, then) {
  if_stat <- lazyeval::expr_text(if_stat)
  then    <- lazyeval::expr_text(then)
  sprintf("ifelse(%s, %s, ", if_stat, then)
}

#' @rdname i
e <- function(else_ret) {
  else_ret <- lazyeval::expr_text(else_ret)
  else_ret
}

#' @rdname i
ie <- function(...) {
  args <- list(...)

  for (i in 1:(length(args) - 1) ) {
    if (substr(args[[i]], 1, 6) != "ifelse") {
      stop("All but the last argument, need to be i functions.", call. = FALSE)
    }
  }
  if (substr(args[[length(args)]], 1, 6) == "ifelse"){
    stop("Last argument needs to be an e function.", call. = FALSE)
  }
  args$final <- paste(rep(')', length(args) - 1), collapse = '')
  eval_string <- do.call('paste', args)
  eval(parse(text = eval_string))
}
