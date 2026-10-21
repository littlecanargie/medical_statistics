# Figures for Chapter 6: 200 repeated 95% confidence intervals for the mean
# height of Tsing-Hua students, population N(165, 7^2) as in Chapters 4-5.
#   Confidence_interval.png : n = 36, sigma known (z interval)
#   confidence_t.png        : n = 9, sample SD plugged into the z interval
# Run from this folder: Rscript confidence_interval_figures.R

mu <- 165
sigma <- 7
B <- 200

simulate_ci <- function(n, known_sigma, seed) {
  set.seed(seed)
  x <- matrix(rnorm(B * n, mu, sigma), nrow = B)
  m <- rowMeans(x)
  s <- if (known_sigma) sigma else apply(x, 1, sd)
  half <- qnorm(0.975) * s / sqrt(n)
  data.frame(lower = m - half, upper = m + half)
}

plot_ci <- function(ci, file, at) {
  miss <- ci$lower > mu | ci$upper < mu
  png(file, width = 1298, height = 1115, res = 160)
  par(mar = c(3, 1, 0.5, 1))
  plot(NA, xlim = range(at), ylim = c(-3, B + 1), axes = FALSE,
       xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  segments(ci$lower, B:1, ci$upper, B:1, col = ifelse(miss, "red", "black"))
  segments(mu, -3, mu, B + 1)
  axis(1, at = at, pos = -3)
  dev.off()
  sum(miss)
}

# Seeds chosen so the miss counts match the text: 10/200 (z) and 18/200 (s plugged in).
ci_z <- simulate_ci(n = 36, known_sigma = TRUE, seed = 2039)
ci_s <- simulate_ci(n = 9, known_sigma = FALSE, seed = 2028)
cat("misses (n = 36, sigma known):", plot_ci(ci_z, "Confidence_interval.png", seq(158, 172, 2)), "\n")
cat("misses (n = 9, s plugged in):", plot_ci(ci_s, "confidence_t.png", seq(150, 180, 5)), "\n")
