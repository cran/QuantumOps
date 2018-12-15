\name{Z}
\alias{Z}
\title{Z}
\usage{
Z(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the Z gate or ket after a Z gate is applied
}

\description{
If no argument is supplied, returns the matrix of Z gate. If ket given as input, applies a Z gate to the input ket and returns the resulting ket
}

\examples{
 Z(ket(1,0))
 Z()
}
