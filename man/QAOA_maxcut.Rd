\name{QAOA_maxcut}
\alias{QAOA_maxcut}
\title{QAOA_maxcut}
\usage{
QAOA_maxcut(connectionMatrix,p=1,gamma=pi/p,beta=pi/(2*p),displayProgress=FALSE)
}
\arguments{
  \item{connectionMatrix}{ Matrix that specifies the edges between nodes. Rows are source nodes and columns are destination nodes. Value of 0 means no edge, value of 1 means edge. If edge is undirected, an edge should be specified going both directions. }

  \item{p}{ Number of iterations that algorithm will run. Each iteration applies U(C,g) and U(B,b)  }

  \item{gamma}{Angle for U(C,g), currently the same for all iterations. Should be between 0 and 2*pi}

  \item{beta}{Angle for U(B,b), currently the same for all iterations. Should be between 0 and pi}

  \item{displayProgress}{Boolean which specifies if progress should be shown. If TRUE, a bar plot is continually updated showing the amplitudes}
}

\value{
Ket after algorithm is applied
}

\description{
Takes a connection matrix as input and converts it to a set of clauses, then runs the Quantum Approximation Optimization Algorithm (Farhi, Goldstone, and Gutmann 2014) <arXiv:1411.4028>. 
}

\examples{
 QAOA_maxcut(randomConnectionMatrix(4,2),p=5)
}
