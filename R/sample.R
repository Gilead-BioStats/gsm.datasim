samle <- function(pool, n, replace) {
  if (n == 1) {
    result <- unlist(pool)
  } else {
    result <- sample(pool, var_inputs$n, replace = TRUE)
  }
  return(result)
}
