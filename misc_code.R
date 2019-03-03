# Some miscellaneous code that ended up not being needed -- may or may not work
# as-is.

library(TSA)

sample_process <- function(n) {
  wn <- ts(rnorm(n), start = -2, end = n)
  process <- wn - 0.7 * zlag(wn, 1) - 0.5 * zlag(wn, 2) - 0.6 * zlag(wn, 3)
  process <- window(process, start = 1)  # Scrap times < 1
  process
}

process <- sample_process(100)


# TODO: Does this work??
r_k <- function(k, process) {
  n <- length(process)
  if (n < k + 1) {
    return(0)
  } else {
    mu <- mean(process)
    head <- window(process, end = n - k)
    tail <- window(process, start = k + 1)
    top <- sum((head - mu) * (tail - mu))
    bottom <- sum((process - mu)^2)
    top / bottom
  }
}

process <- sample_process(100)
r_k(5, process)
