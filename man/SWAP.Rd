\name{SWAP}
\alias{SWAP}
\title{SWAP}
\usage{
SWAP(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matrix of the SWAP gate or ket after an SWAP gate is applied
}

\description{
If no argument is supplied, returns the matrix of SWAP gate. If ket given as input, applies an SWAP gate to the input ket and returns the resulting ket
}

\examples{
 SWAP(ket(0,1,0,0))
 SWAP()
}
