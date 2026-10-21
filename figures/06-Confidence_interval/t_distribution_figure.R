# Figure for Chapter 6: density of Student's t distribution for several
# degrees of freedom, compared with the standard normal (thick black line).
# Run from this folder: Rscript t_distribution_figure.R

x <- seq(-4, 4, length.out = 801)
df <- c(1, 4, 8, 12)
cols <- c("red", "blue", "darkgreen", "magenta")

png("student.png", width = 1400, height = 1000, res = 200)
par(mar = c(3, 3.5, 0.5, 0.5), mgp = c(2, 0.6, 0), las = 1)
plot(x, dnorm(x), type = "n", ylim = c(0, 0.42), xlab = "", ylab = "",
     xaxs = "i", yaxs = "i", bty = "l")
for (i in seq_along(df)) lines(x, dt(x, df[i]), col = cols[i], lwd = 1.5)
lines(x, dnorm(x), lwd = 3)
legend("topright", bty = "n", lwd = c(rep(1.5, length(df)), 3),
       col = c(cols, "black"),
       legend = c(sapply(df, function(v) as.expression(bquote(nu == .(v)))),
                  expression(N(0, 1))))
dev.off()
