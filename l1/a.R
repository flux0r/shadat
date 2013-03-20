omit.inf <- function(xs) {
	return(xs[is.finite(xs)])
}

countLessThan <- function(x, ys) {
	return(sum(na.omit(ys) <= x))
}

"%.%" <- function(f, g) {
	return(function(...) {
		return(f(g(...)))
	})
}

finiteMax <- max %.% omit.inf %.% na.omit

pdf(file="g.pdf")

# 4 a
ckm <- read.csv("ckm.csv")

# 4 b
# Adopters is a vector containing the number of people who had adopted by a
# given date with the date given by the index of the vector. It ends at 17
# because that is the maximum finite date in the data set variable.
adopters <- sapply(1:finteMax(ckm$adoption_date), function(x) {
		return(countLessThan(x, ckm$adoption_date))
	})

# 4 c
new <- diff(c(0, adopters))
adopters <- c(0, adopters[1:length(new) - 1])
p <- plot(adopters, new)

# 4 d
fit <- lm(new ~ adopters)
abline(fit, col=hsv(0, 1, 0, 0.4))

# 4 d
#
# The first two bandwidths suggest monotonicity, while the third does not.
lines(ksmooth(adopters, new, "normal", bandwidth=200),
	col=hsv(0.65, 1, 0.5, 0.8))
lines(ksmooth(adopters, new, "normal", bandwidth=80),
	col=hsv(0.90, 0.6, 0.5, 0.8))
lines(ksmooth(adopters, new, "normal", bandwidth=25),
	col=hsv(0.45, 0.6, 0.5, 0.8))

# 4 e
#
# The residuals do not appear to be independent of the predictor.
p.diag <- plot(adopters, residuals(fit))
lines(ksmooth(adopters, residuals(fit), "normal", bandwidth=200),
	col=hsv(0.65, 1, 0.5, 0.8))
lines(ksmooth(adopters, residuals(fit), "normal", bandwidth=80),
	col=hsv(0.90, 0.6, 0.5, 0.8))
lines(ksmooth(adopters, residuals(fit), "normal", bandwidth=25),
	col=hsv(0.45, 0.6, 0.5, 0.8))

dev.off()
