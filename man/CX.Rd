\name{CX}
\alias{CX}
\title{CX}
\usage{
CX(...)
}
\arguments{
  \item{...}{ No argument, or 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
Matix of the Controlled-X gate or ket after a Controlled-X gate is applied
}

\description{
If no argument is supplied, returns the matrix of Controlled-X gate. If ket given as input, applies a Controlled-X gate to the input ket and returns the resulting ket
}

\examples{
 CX(ket(1,1,1,1))
 CX()
}
