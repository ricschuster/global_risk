#' Calculate targets
#'
#' This function sets area-based targets for a species' ESH---or a part of
#' it---using the Rodriguez equations. Additional arguments are also used
#' to specify a cap.
#'
#' @param x \code{numeric} vector of containing the area ESH distributions.
#'  Note that the areas are assumed to be in square kilometers.
#'
#' @param minimum.range.size \code{numeric} area
#'   at which distributions are given the proprotional target specified
#'   in argument to \code{proportion.at.minimum.range.size}. The units
#'   are supplied in square kilometers.
#'
#' @param proportion.at.minimum.range.size \code{numeric} proportin of the
#'  distribution that needs to be preserved when the size of the distriubtion
#'  is equal to or larger than the area specified in \code{maximum.range.size}.
#'
#' @param maximum.range.size \code{numeric} area
#'   at which distributions are given the proprotional target specified
#'   in argument to \code{proportion.at.maximum.range.size}. The units
#'   are supplied in square kilometers.
#'
#' @param proportion.at.maximum.range.size \code{numeric} proportion of the
#'  distribution that needs to be preserved when the size of the distribution
#'  is equal to or larger than the area specified in \code{maximum.range.size}.
#'
#' @param cap.range.size The range size threshold at which the cap is applied.
#'   The units are in square kilometers.
#'
#' @param cap.target.amount The target amount of area assigned to species
#'   with distributions greater than \code{cap.range.size}. The units are in
#'   square kilometers.
#'
#' @details Note that \code{NA} or \code{Inf} areas will be given a target
#'   of \code{NA}.
#'
#' @return \code{\link{numeric}} vector with the target proportion of the #'
#'   distributions.
#'
#' @details The default settings correspond to those given in Runge et al. 2015.
#'
#' @examples
#' ranges <- seq(1, 50000000, length.out = 1000)
#' targets <- calculate.targets(ranges)
#' plot(targets ~ ranges, xlab = "Range size", ylab = "Protection target (%)")
calculate_targets <- function(x, minimum.range.size = 1000,
                              proportion.at.minimum.range.size = 1,
                              maximum.range.size = 250000,
                              proportion.at.maximum.range.size = 0.1,
                              cap.range.size = 10000000,
                              cap.target.amount = 1000000) {
  # initialization
  out <- rep(NA_real_, length(x))
  pos <- which(is.finite(x))
  # set targets
  out[pos] <- stats::approx(x = c(minimum.range.size, maximum.range.size),
                            y = log(c(proportion.at.minimum.range.size,
                                      proportion.at.maximum.range.size)),
                            xout = x[pos], method = "linear", rule = 2)$y
  out[pos] <- exp(out[pos])
  # set caps
  cap_pos <- which(x > cap.range.size)
  if (length(cap_pos) > 0) {
    out[cap_pos] <- cap.target.amount / x[cap_pos]
  }
  # return values
  return(out)
}
