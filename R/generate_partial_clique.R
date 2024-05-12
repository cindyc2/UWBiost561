# generate_partial_clique.R


#' Generate Random Graph
#' This package is based off of the generate random clique function.
#' It simply removes 1 edge from the full clique.
#'
#' @param n  # the number of nodes requested in the graph (soon to be matrix)
#' n = number of nodes in the graph (along the diagonal of the matrix)
#'  i.e. to represent this, it result in n rows and also n columns to show the connections
#'  and the matrix will be n*n
#' n  # choose this number in the function
#' @param clique_fraction  #fraction of the total nodes in the graph connected in our partial clique
#' clique_fraction  # choose this number in the function. A fully connected clique will have clique faction =1.
#' @param clique_edge_density # edge density means proportion of this clique that is connected.
#' A fully connected clique will have clique_edge_density = 1
#' # choose this number in the function
#'
#' @return adj_mat # a newly-created adjacency matrix that fit the specifications given above
#' @export
#'
#' @examples
generate_partial_clique <- function(n, clique_fraction, clique_edge_density){
  set.seed(0)

  # per Kevin's OH, we can assign density_low to anything
density_low <-  clique_edge_density/2

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
    # sample_idx <- sample(1:n)
    # adj_mat <- adj_mat[sample_idx, sample_idx]
    #
    # Compute the appropriate reverse order
    # rev_order <- sapply(1:n, function(i){
    #   which(sample_idx == i)
    # })
    # # To see what happens, try: adj_mat[rev_order, rev_order]

 # remove part of the clique, the first cell R will try to put it in
    # this will ensure we will be in the clique regardless of size
    adj_mat[2, 1] <- 0
    adj_mat[1, 2] <- 0

    return(list(adj_mat = adj_mat
                # ,
                # rev_order = rev_order
                ))


  adj_mat <- result$adj_mat


  m=round(n*clique_fraction)
  # this function automatically does this:
  #symmetric, only 0, 1, 2 in matrix
  # only 1 along diagonols,
  # no rows or column names
  # have at least clique fraction

    stopifnot( n %% 1 == 0, n >= 0,
               isSymmetric(adj_mat) == TRUE,
               all(adj_mat == 0 | adj_mat == 1 | adj_mat == 2 ),
               diag(adj_mat == 1),
               length(rownames(adj_mat)) == 0,
               length(colnames(adj_mat)) == 0,
              nrow(adj_mat)*ncol(adj_mat) >= 0,
             round(n*clique_fraction)/n >= 0,  # had to divide by n to get between 0,1
             round(n*clique_fraction)/n <= 1,
             round(clique_edge_density*m*(m-1)/2)/m >= 0,
             round(clique_edge_density*m*(m-1)/2)/m <= 1
             )
# some of these could go up top, but the ones that evaluate the adj_mat have to be done
    # after we make it


  }


# helper functions
# .helper
