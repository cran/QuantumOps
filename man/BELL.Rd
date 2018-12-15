\name{BELL}
\alias{BELL}
\title{BELL}
\usage{
BELL(...)
}
\arguments{
  \item{...}{ No argument, or 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
Matix of the BELL gate or ket after a BELL gate is applied
}

\description{
If no argument is supplied, returns the matrix of BELL gate. If ket given as input, applies a BELL gate to the input ket and returns the resulting ket
}

\examples{
 BELL(ket(1,1,1,1))
 BELL()
}
