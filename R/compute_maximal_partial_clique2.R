


#' compute_maximal_partial_clique. Package that computes the largest partial clique given an adjacency matrix adj_mat and a required edge density alpha
#' # Starting with the largest possible clique, we determine if that is a full clique. If not, we calculate the number of edges and compare it to the level of alpha requested.
#' # If that is still not large enough, we remove one node and repeat the process of assessing full cliques and partial cliques until we reach teh alpha level.
#' # It is possible the largest clique above the alpha level is 1 node with 100% connected to itself.

#'
#' @param adj_mat # adjacency matrix to be tested when loaded into the function
#' @param alpha # the level of partial clique requested
#' ! I think alpha is the edge density?
#' @param n # the number of nodes (and the number of rows (or columns) in the adjacency matrix
#'
#' @return # a list with the clique index of the largest partial clique in the adjacency matrix, and its edge density
#' @export
#'
#' @examples
compute_maximal_partial_clique <- function(adj_mat, alpha, n){
  adj_mat <- my_results$adj_mat

  stopifnot(isSymmetric(adj_mat) == TRUE,
            all(adj_mat == 0 | adj_mat == 1 | adj_mat == 2 ),
            diag(adj_mat == 1),
            #3 !how do I do this? has no row- or column-names
            (nrow(adj_mat) >= 5 & nrow(adj_mat) <= 50),
            (ncol(adj_mat) >= 5 & ncol(adj_mat) <= 50),
            length(alpha) ==1,
            alpha >= 0.5 & alpha <= 1,
  )


  # size n
  # length of matrix will give total number of cells, so we need square root to get back to n
  n = nrow(adj_mat) # or ncol(adj_mat) because they have to be symmetric # or sqrt(length(adj_mat))

  # to find maximum start at the largest possible set of nodes to see if it is a full clique
  for(i in n:1){
    # summing the matrix is another way to count the number of nodes since the 1 represents an edge
    # it repeats itself so you just want half? !Or something more exact?
    if(a  == (n*(n-1))/2,
       return(list(clique_idx = clique_idx,
                   edge_density = edge_density))
       # if not full clique, see if it has enough edges to be above the clique fraction
       else(if( (sum(adj_mat) -n)/2 >= clique_fraction,
                click_idx <- (partitial_click_idx))
         return(list(clique_idx = clique_idx,
                     edge_density = edge_density))
         # if not a large enough partial clique, remove 1 node at a time then compute if that is a full clique
         # or a large enough partial clique
         else(for(j in 1:n){
           #!delete j nodes then compute
           sub_mat_j <- adj_mat[-j,-j]
           # !we need to do this n choose j times becuase the forloop will go through each node
           #
           if(sum(sub_mat_j) -n)/2  == (n-1)*(n-2))/2),
       clique_idx <- sub_mat_j_clique_idx,
       return(list(clique_idx = clique_idx,
                   edge_density = edge_density))
       # keep repeating with
       else( #repeat loop)
         if((n-j)==1, return(clique_idx == "n separate nodes, largest clique is 1 node")
         })
       ))
  }

  )
}



# this will not pass the speed test.
# also, depending on the alpha level and the number of nodes, there may be 2 smaller cliques that my test
# catergorizes as one large clique so it would be a false positive ID of a partial clique

# All the algebra, combinatorics and advanced math I had to think through for this problem

# there is only n choose n (or 1) way to have the full node connected
# n choose n-1 is symmetric with n choose 1 (or n ways) to test
# If we tried to start smaller  with cliques size 1, we would have to test the higher levels anyways
#to make sure there is no larger clique
#I think we will have n! ways to test this! But I can't see a cleaner way to do this.


#the maximum clique is actually n*n-n,  the n*n matrix with the n nodes removed
#becauase we aren't considering connecting to itself
#but this can be converted into your formula
#n*(n-1)/2

# why are you using n choose 2 when these are triangle numbers?
#To calculate the maximum number of nodes of a 10 node graph, you want the sum of 1:n-1
# 9+8+7+6+5+4+3+2+1
# I found out this can also be expressed as n choose 2!

# all of this is to say, its not for lack of effort.

# helper functions
# .helper
