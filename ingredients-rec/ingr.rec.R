# creates a ingredient recommendation object
# which is a similarity matrix and a counts vector
create_sim <- function(mat) {
  counts <- colSums(mat)
  ingr <- colnames(mat)

  # recipes are scaled so that recipes with many ingredients contribute less to similarities
  mat <- t(mat / sqrt(rowSums(mat)))

  sim <- mat / sqrt(rowSums(mat * mat)) # normalize item vectors
  sim <- sim %*% t(sim) # gram matrix
  colnames(sim) <- ingr

  res <- list("sim" = sim, "counts" = counts)
  class(res) <- "ingr.rec"

  res
}

# predict method for ingr.rec object
# chr indicates whether newdata is a vector of ingredients
# or a binary vector
# pow indicates concordance of the ingredients (1 for none and close to 0 for high)
# adventurous indicates how much to boost less frequently used items
predict.ingr.rec <- function(self, newdata, pow, chr = TRUE, adventurous = 0, n = 10) {
  curr_row <- newdata

  if (chr) {
    curr <- Filter(curr_row, f = \(x) x %in% colnames(self$sim))

    curr_row <- self$sim %>%
      colnames() %>%
      map_lgl(~ . %in% curr)
  } else {
    curr <- colnames(self$sim)[curr_row]

    curr_row <- as.logical(curr_row)
  }

  sims <- self$sim[curr_row, ]^pow

  if (length(curr) > 1) {
    # only colsum if more than 1 ingredient
    sims <- colSums(sims)
  }

  if (length(curr) == 0) {
    tibble(base = 0.0, spice = 0.0, total = 0.0, name = c("NONE"))
  } else {
    tibble(
      base = sims - min(sims),
      spice = adventurous * 0.05 * (log(max(self$counts)) - log(self$counts)),
      total = base + spice,
      name = colnames(self$sim)
    ) %>%
      arrange(-total) %>%
      filter(!(name %in% curr)) %>%
      head(n)
  }
}
