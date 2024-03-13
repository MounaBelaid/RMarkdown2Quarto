# appends a numeric suffix to non-unique selected file names
addNumericSuffix <- function(vec) {
  counts <- table(vec)
  duplicates <- names(counts[counts > 1])

  for (dup in duplicates) {
    indices <- which(vec == dup)
    if (length(indices) > 1) {
      for (i in seq_along(indices)[-1]) {
        vec[indices[i]] <- paste0(dup, i)
      }
    }
  }

  return(vec)
}
