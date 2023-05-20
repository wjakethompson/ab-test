# Implementation from: https://stats.stackexchange.com/a/112671/188170

#' Logistic transformation of the Beta CDF
#'
#' @param alpha First shape parameter for Beta distribution
#' @param beta Second shape parameter for Beta distribution
#' @param x Proportion value to be transformed
#' @param lower Lower bound (if numbers have been rescaled)
#' @param upper Upper bound (if numbers have been rescaled)
#'
#' @noRd
f_beta <- function(alpha, beta, x, lower = 0, upper = 1) {
  p <- stats::pbeta((x - lower) / (upper - lower), alpha, beta)
  log(p / (1 - p))
}

#' Sums of squares
#'
#' @param fit Estimated quantile values
#' @param actual Specified quantile values
#'
#' @noRd
delta <- function(fit, actual) {
  sum((fit - actual) ^ 2)
}

#' Title
#'
#' The objective function handles the transformed parameters `theta` and
#' uses `f_beta` and `delta` to fit the values and measure their discrepancies.
#'
#' @param theta Parameters to optimize
#' @param x The values of quantiles specified in `prob`
#' @param prob Quantiles associated with each value of `x`
#' @param ... Additional arguments passed to `f_beta()`
#'
#' @noRd
objective <- function(theta, x, prob, ...) {
  # Parameters are the *logs* of alpha and beta
  ab <- exp(theta)
  fit <- f_beta(ab[1], ab[2], x, ...)
  delta(fit, prob)
}

quiet_nlm <- purrr::quietly(nlm)

#' Find Beta distribution shape parameters
#'
#' Given two proportions and their quantile values, find the Beta shape
#' parameters that represent the distribution.
#'
#' @param x1 First proportion
#' @param p1 Quantile value of first proportion
#' @param x2 Second proportion
#' @param p2 Quantile value of second proportion
#'
#' @return A list with two elements representing the two shape parameters of
#'   the Beta distribution.
#' @export
#'
#' @examples
#' # Find the Beta distribution where the 2.5 percentile is 0.08 and the 97.5
#' # percentile is 0.42.
#' calc_beta(x1 = 0.08, p1 = 0.025, x2 = 0.42, p2 = 0.975)
calc_beta <- function(x1 = 0.05, p1 = 0.025, x2 = 0.95, p2 = 0.975) {
  x1 <- check_double(x1, lb = 0, ub = 1, inclusive = FALSE, name = "x1")
  p1 <- check_double(p1, lb = 0, ub = 1, inclusive = FALSE, name = "p1")
  x2 <- check_double(x2, lb = 0, ub = 1, inclusive = FALSE, name = "x2")
  p2 <- check_double(p2, lb = 0, ub = 1, inclusive = FALSE, name = "p2")

  x <- c(x1, x2)
  x_p <- logit(c(p1, p2))

  sol <- quiet_nlm(f = objective, p = log(c(1, 1)),
                   x = x, prob = x_p, lower = 0, upper = 1,
                   typsize = c(1, 1), fscale = 1e-12, gradtol = 1e-12)
  shapes <- exp(sol$result$estimate)
  list(shape1 = shapes[1], shape2 = shapes[2])
}

#' Beta distribution logit scale prior
#'
#' For a given Beta distribution find the mean and standard deviation of a
#' Normal distribution that can be used as a prior for a GLM.
#'
#' @param shape1 First shape parameter for Beta distribution
#' @param shape2 Second shape parameter for Beta distribution
#' @param n The number of draws to make from the Beta distribution
#'
#' @return A list with two elements representing the location and scale
#'   parameters of the Normal distribution.
#' @export
#'
#' @examples
#' logit_beta(5, 17, n = 1e4)
logit_beta <- function(shape1, shape2, n = 1e4) {
  shape1 <- check_double(shape1, name = "shape1")
  shape2 <- check_double(shape2, name = "shape2")
  n <- check_integer(n, lb = 0, inclusive = FALSE, name = "n")

  beta_draws <- stats::rbeta(n = n, shape1 = shape1, shape2 = shape2)
  logit_draws <- logit(beta_draws)
  list(location = mean(logit_draws), scale = stats::sd(logit_draws))
}
