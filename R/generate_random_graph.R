function(n,
         clique_fraction = 0.2,
         density_low = 0.1){
  # Check all the arguments are correct
  stopifnot(n %% 1 == 0, n >= 0,
            clique_fraction >= 0, clique_fraction <= 1,
            density_low >= 0, density_low <= 1)

  # Generate an unsymmetric matrix
  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(1-density_low, density_low),
                           replace = TRUE),
                    nrow = n, ncol = n)

  # Symmetrize the matrix
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1

  # Form the clique
  clique_size <- ceiling(n * clique_fraction)
  adj_mat[1:clique_size, 1:clique_size] <- 1

  # Randomize the order of the nodes
  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]

  # Compute the appropriate reverse order
  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })
  # To see what happens, try: adj_mat[rev_order, rev_order]

  return(list(adj_mat = adj_mat,
              rev_order = rev_order))
}
