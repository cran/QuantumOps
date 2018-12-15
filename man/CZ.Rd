\name{CZ}
\alias{CZ}
\title{CZ}
\usage{
CZ(...)
}
\arguments{
  \item{...}{ No argument, or 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
Matix of the Controlled-Z gate or ket after a Controlled-Z gate is applied
}

\description{
If no argument is supplied, returns the matrix of Controlled-Z gate. If ket given as input, applies a Controlled-Z gate to the input ket and returns the resulting ket
}

\examples{
 CZ(ket(1,1,1,1))
 CZ()
}
