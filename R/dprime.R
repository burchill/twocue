
#' Calculate d'
#'
#' Calculates d'
#'
#' @param n_hit hits = true positives
#' @param n_miss misses = false negatives
#' @param n_fa false alarms = false positives
#' @param n_cr correctly rejected = true negatives
#' @export
dprime <- function(n_hit, # hits = true positives
                   n_miss,# misses = false negatives
                   n_fa,  # false alarms = false positives
                   n_cr   # correctly rejected = true negatives
) {
  # Ratios
  hit_rate <- n_hit/(n_hit + n_miss)
  fa_rate <- n_fa/(n_fa + n_cr)

  # Adjusted ratios
  hit_rate_adjusted <- (n_hit+0.5)/((n_hit+0.5) + n_miss + 1)
  fa_rate_adjusted <- (n_fa+0.5)/((n_fa+0.5) + n_cr + 1)

  qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)
}
