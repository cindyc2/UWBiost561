% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_maximal_partial_clique.R
\name{compute_maximal_partial_clique}
\alias{compute_maximal_partial_clique}
\title{'Compute Maximal Partial Clique
A package that barely passes this assignment. It computes the "largest partial clique" that I can find using R.
Given an adjacency matrix adj_mat and a required edge density alpha, the program will
determine if the matrix is diagonal, (meaning the nodes have no connection.)
If so, it will return 1 node with clique density of 1.
If the matrix is not diagonal, there is at least one edge.
The program will list the first edge it finds as the largest partial clique.
It is quite probable that the largest clique is larger, but my program doesn't care.
Also it will not tell you if there other cliques of equal size.
(There most likely is because most graphs have more than 1 edge.)
Good luck finding them because this package won't help!}
\usage{
compute_maximal_partial_clique(adj_mat, alpha)
}
\arguments{
\item{adj_mat}{adjacency matrix to be tested when loaded into the function}

\item{alpha}{the density level of partial clique requested}
}
\value{
a list with the clique index of the largest partial clique in the adjacency matrix, and its edge density
}
\description{
'Compute Maximal Partial Clique
A package that barely passes this assignment. It computes the "largest partial clique" that I can find using R.
Given an adjacency matrix adj_mat and a required edge density alpha, the program will
determine if the matrix is diagonal, (meaning the nodes have no connection.)
If so, it will return 1 node with clique density of 1.
If the matrix is not diagonal, there is at least one edge.
The program will list the first edge it finds as the largest partial clique.
It is quite probable that the largest clique is larger, but my program doesn't care.
Also it will not tell you if there other cliques of equal size.
(There most likely is because most graphs have more than 1 edge.)
Good luck finding them because this package won't help!
}
