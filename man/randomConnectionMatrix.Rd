\name{randomConnectionMatrix}
\alias{randomConnectionMatrix}
\title{randomConnectionMatrix}
\usage{
randomConnectionMatrix(nNodes,nEdges)
}
\arguments{
  \item{nNodes}{ Number of nodes in generated graph   }

  \item{nEdges}{ Number of undirected edges in generated graph }
}

\value{
Connection Matrix specifying the edges of an undirected graph. Rows are source nodes, columns are destination nodes.
}

\description{
Generates a connection matrix for a random undirected graph. Intended for input to QAOA_maxcut.
}

\examples{
 randomConnectionMatrix(5,3)
}
