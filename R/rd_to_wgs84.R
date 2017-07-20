#' Convert RD to WGS84
#' 
#' Convert the (X, Y) of the Dutch standard RD (Rijksdriehoek) to the (lat, long)
#' of WGS84, as used in GPS.
#' 
#' @param X the X coordinate (mapped to longitude)
#' @param Y the Y coordinate (mapped to latitude)
#' 
#' @return A double of length 2, the lat and the long.
#' 
#' @source http://home.solcon.nl/pvanmanen/Download/Transformatieformules.pdf
#' Formula 6
#' 
#' @example 
#' # From the source article:
#' # Westertoren Amsterdam
#' rd_to_wgs84(120700.723, 487525.501)
#' 
#' # Martinitoren Groningen:
#' rd_to_wgs84(233883.131, 582065.167)

rd_to_wgs84 <- function(X, Y) {
  X_0      <- 155000
  Y_0      <- 463000
  dX <- (X - X_0) * 10^-5
  dY <- (Y - Y_0) * 10^-5
  data_frame(phi    = get_phi_or_lamdba(dX, dY, 52.15517440, phi_table()),
             lambda = get_phi_or_lamdba(dX, dY, 5.38720621, lambda_table()))
}

get_phi_or_lamdba <- function(dX, dY, letter_not, tble) {
  library(dplyr)
  dX_mat <- outer(dX, tble$p, "^")
  dY_mat <- outer(dY, tble$q, "^")
  pq_mat <- rep(tble$pq, nrow(dX_mat)) %>% matrix(nrow = nrow(dX_mat), byrow = TRUE)
  letter_not + rowSums(pq_mat * dX_mat * dY_mat) / 3600
}

phi_table <- function() {
  dplyr::tribble(
    ~p, ~q, ~pq,
    0, 1, 3235.65389,
    2, 0, -32.58297 ,
    0, 2, -0.24750,
    2, 1, -0.84978,
    0, 3, -0.06550,
    2, 2, -0.01709,
    1, 0, -0.00738,
    4, 0, 0.00530 ,
    2, 3, -0.00039,
    4, 1, 0.00033,
    1, 1, -0.00012
  )
}

lambda_table <- function() {
  dplyr::tribble(
    ~p, ~q, ~pq,
    1, 0, 5260.52916,
    1, 1, 105.94684,
    1, 2, 2.45656,
    3, 0, -0.81885,
    1, 3, 0.05594,
    3, 1, -0.05607,
    0, 1, 0.01199,
    3, 2, -0.00256,
    1, 4, 0.00128,
    0, 2, 0.00022,
    2, 0, -0.00022,
    5, 0, 0.00026
  )
}
