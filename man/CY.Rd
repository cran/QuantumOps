\name{CY}
\alias{CY}
\title{CY}
\usage{
CY(...)
}
\arguments{
  \item{...}{ No argument, or 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
Matix of the Controlled-Y gate or ket after a Controlled-Y gate is applied
}

\description{
If no argument is supplied, returns the matrix of Controlled-Y gate. If ket given as input, applies a Controlled-Y gate to the input ket and returns the resulting ket
}

\examples{
 CY(ket(1,1,1,1))
 CY()
}
