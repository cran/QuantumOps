\name{S}
\alias{S}
\title{S}
\usage{
S(...)
}
\arguments{
  \item{...}{  No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the S gate or ket after an S gate is applied
}

\description{
If no argument is supplied, returns the matrix of S gate. If ket given as input, applies an S gate to the input ket and returns the resulting ket
}

\examples{
 S(ket(1,1))
 S()
}
