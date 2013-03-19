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

ckm <- read.csv("ckm.csv")

adopters <- sapply(1:max(omit.inf(ckm$adoption_date)), function(x) {
		return(countLessThan(x, ckm$adoption_date))
	})
