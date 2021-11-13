create_sim <- function(mat) {
  # pt <- df %>%
  #   distinct(id, base) %>%
  #   add_count(base) %>%
  #   filter(n >= 2) %>%
  #   select(-n) %>%
  #   group_by(id, base) %>%
  #   summarize(n = 1) %>%
  #   ungroup() %>%
  #   pivot_wider(names_from = "base", values_from = n, values_fill = 0) %>%
  #   select(-id)
  # 
  # mat <- as.matrix(pt)
  counts <- colSums(mat)
  ingr <- colnames(mat)
  
  mat <- t(mat / sqrt(rowSums(mat * mat)))
  sim <- mat / sqrt(rowSums(mat * mat))
  sim <- sim %*% t(sim)
  colnames(sim) <- ingr

  res <- list("sim" = sim, "counts" = counts)
  class(res) <- "ingr.rec"

  res
}

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
