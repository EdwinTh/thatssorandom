#' Check if the id columns are unique
#'
#' @param x a data frame
#' @param ... the unquoted column names of the id variables
#'
#' @return
#' If the id variables are unique TRUE. Else, the rows in `x` for which the id
#' variables are not unique, arranged by id variables.
#'
#' @examples
#' mtcars %>% unique_id(cyl, disp)
#' mtcars %>% unique_id(cyl, wt)
#' mtcars %>% unique_id(cyl, qsec)
#' @export
unique_id <- function(x, ...) {
  id_set <- x %>% select(...)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>%
      filter(id_set %>% duplicated()) %>%
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange(...)
    )
  }
}
