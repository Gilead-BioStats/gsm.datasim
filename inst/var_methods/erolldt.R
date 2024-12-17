enrolldt <- function(n, startDate, endDate, enrollyn_dat) {
  full_sample <- sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
  full_sample[enrollyn_dat == "N"] <- NA
  return(full_sample)
}
