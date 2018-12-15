\name{TOFFOLI}
\alias{TOFFOLI}
\title{TOFFOLI}
\usage{
TOFFOLI(...)
}
\arguments{
  \item{...}{ No argument, or an 8 dimensional (3 qubit) ket (column vector) that is input to the gate   }
}

\value{
Matix of the TOFFOLI gate or ket after a TOFFOLI gate is applied
}

\description{
If no argument is supplied, returns the matrix of TOFFOLI gate. If ket given as input, applies a TOFFOLI gate to the input ket and returns the resulting ket
}

\examples{
 TOFFOLI(ket(1,1,1,1,0,1,0,1))
 TOFFOLI()
}
