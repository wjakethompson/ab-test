abort_bad_argument <- function(arg, must, not = NULL, extra = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}")
  }
  if (!is.null(extra)) {
    msg <- glue::glue("{msg}", "{extra}", .sep = "\n")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not)
}

check_double <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE, name) {
  if (inclusive) {
    check_lb <- function(.x, .y) x < .y
    check_ub <- function(.x, .y) x > .y
  } else if (!inclusive) {
    check_lb <- function(.x, .y) x <= .y
    check_ub <- function(.x, .y) x >= .y
  }

  if (!is.numeric(x)) {
    abort_bad_argument(name, must = "be a numeric scalar", not = typeof(x))
  }

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x)) {
    abort_bad_argument(name, must = "be non-missing")
  }

  if (check_lb(x, lb) || check_ub(x, ub)) {
    msg <- if (is.infinite(lb)) {
      glue::glue("be less than {ub}")
    } else if (is.infinite(ub)) {
      glue::glue("be greater than {lb}")
    } else {
      glue::glue("be between {lb} and {ub}")
    }
    abort_bad_argument(name, must = msg)
  }

  x
}
check_double <- Vectorize(check_double)

check_integer <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE,
                          allow_null = FALSE, name) {
  if (is.null(x) && allow_null) return(x)

  if (inclusive) {
    check_lb <- lb
    check_ub <- ub
  } else if (!inclusive) {
    check_lb <- lb + 1L
    check_ub <- ub - 1L
  }

  if (!is.numeric(x)) {
    abort_bad_argument(name, must = "be a numeric scalar", not = typeof(x))
  }
  x <- as.integer(x)

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x)) {
    abort_bad_argument(name, must = "be non-missing")
  }

  if (x < check_lb || x > check_ub) {
    msg <- if (is.infinite(lb)) {
      glue::glue("be less than {ub}")
    } else if (is.infinite(ub)) {
      glue::glue("be greater than {lb}")
    } else {
      glue::glue("be between {lb} and {ub}")
    }
    abort_bad_argument(name, must = msg)
  }

  x
}
