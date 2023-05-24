fmt_prop_pct <- function(x, digits = 0, fmt_small = TRUE) {
  x <- check_bound_real(x, name = "x", lb = 0, ub = 1)
  digits <- check_0_int(digits, name = "digits")
  
  x_chr <- (x * 100) %>%
    fmt_digits(digits)
  
  if (fmt_small) {
    small <- 1 / (10 ^ digits)
    small_text <- small %>%
      fmt_digits(digits) %>%
      paste0_after(.first = "<")
    
    large <- 100 - small
    large_text <- large %>%
      fmt_digits(digits) %>%
      paste0_after(.first = ">")
    
    x_chr[round(x * 100, digits = digits) < small] <- small_text
    x_chr[round(x * 100, digits = digits) > large] <- large_text
  }
  
  return(x_chr)
}

fmt_digits <- function(x, digits = 3, fmt_small = FALSE, max_value = NULL,
                       keep_zero = FALSE) {
  x <- check_number(x, name = "x")
  digits <- check_0_int(digits, name = "digits")
  
  round_x <- round(x, digits)
  to_print <- sprintf("%.*f", digits, round_x)
  
  if (fmt_small) {
    small <- 1 / (10 ^ digits)
    small_text <- sprintf("%.*f", digits, small) %>%
      paste0_after(.first = "<")
    
    to_print[round(x, digits) < small] <- small_text
    
    if (!is.null(max_value)) {
      large <- max_value - small
      large_text <- sprintf("%.*f", digits, large) %>%
        paste0_after(.first = ">")
      
      to_print[round(x, digits) > large] <- large_text
    }
  }
  
  if (keep_zero) {
    to_print[x == 0] <- sprintf("%.*f", digits, 0)
  }
  
  to_print[is.na(x)] <- NA_character_
  
  return(to_print)
}

paste0_after <- function(..., .first) {
  paste0(.first, ...)
}

theme_wjake <- function(base_family = "Source Sans Pro", base_size = 11.5,
                        ...) {
  ret <- hrbrthemes::theme_ipsum(base_family = base_family,
                                 base_size = base_size, ...)
  
  ret <- ret +
    ggplot2::theme(legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   legend.position = "bottom",
                   strip.text.x = ggtext::element_markdown(),
                   strip.text.y = ggtext::element_markdown(),
                   axis.title.x = ggtext::element_markdown(),
                   axis.title.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown(color = "black"),
                   axis.text.y = ggtext::element_markdown(color = "black"))
  
  ret
}

palette_wjake <- c("#009FB7", "#FED766", "#272727", "#696773", "#F0F0F0")
