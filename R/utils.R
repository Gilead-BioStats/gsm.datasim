count_gen <- function(max_n, SnapshotCount) {
  iteration <- max_n %/% SnapshotCount

  counts <- c()
  for (i in seq(SnapshotCount)) {
    if (i > 1) {
      start <- counts[i - 1]
    } else {
      start <- 1
    }

    end <- i * iteration


    if (i < SnapshotCount) {
      counts <- c(counts, sample(start:end, size = 1))
    } else {
      counts <- c(counts, max_n)

    }
  }

  return(counts)
}
