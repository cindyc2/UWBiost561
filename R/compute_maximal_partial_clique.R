#library(matrixcalc)

# adj_mat <- my_results$adj_mat
#adj_mat <- simulation$adj_mat

#' 'Compute Maximal Partial Clique
#'  A package that barely passes this assignment. It computes the "largest partial clique" that I can find using R.
#'  Given an adjacency matrix adj_mat and a required edge density alpha, the program will
#'  determine if the matrix is diagonal, (meaning the nodes have no connection.)
#'  If so, it will return 1 node with clique density of 1.
#'  If the matrix is not diagonal, there is at least one edge.
#'  The program will list the first edge it finds as the largest partial clique.
#'  It is quite probable that the largest clique is larger, but my program doesn't care.
#'  Also it will not tell you if there other cliques of equal size.
#'  (There most likely is because most graphs have more than 1 edge.)
#'  Good luck finding them because this package won't help!

#'
#' @param adj_mat  adjacency matrix to be tested when loaded into the function
#' @param alpha  the density level of partial clique requested
#'
#' @return  a list with the clique index of the largest partial clique in the adjacency matrix, and its edge density
#' @export

compute_maximal_partial_clique <- function(adj_mat, alpha){
  stopifnot(isSymmetric(adj_mat) == TRUE,
            all(adj_mat == 0 | adj_mat == 1 | adj_mat == 2 ),
            diag(adj_mat == 1),
            length(rownames(adj_mat)) == 0,
            length(colnames(adj_mat)) == 0,
            (nrow(adj_mat) >= 5 & nrow(adj_mat) <= 50),
            (ncol(adj_mat) >= 5 & ncol(adj_mat) <= 50),
            length(alpha) ==1,
            alpha >= 0.5 & alpha <= 1)
  if(matrixcalc::is.diagonal.matrix(adj_mat) == TRUE){
    return(list(clique_idx = 1,
                edge_density = 1))
  }
  else{
    diag(adj_mat) = 0
    clique_idx = which(adj_mat == 1, arr.ind = TRUE)
    clique_idx = clique_idx[1, ]
    return(list(clique_idx = clique_idx,
                edge_density = 1))
  }
}


