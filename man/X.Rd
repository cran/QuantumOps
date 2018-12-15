\name{X}
\alias{X}
\title{X}
\usage{
X(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the X gate or ket after an X gate is applied
}

\description{
If no argument is supplied, returns the matrix of X gate. If ket given as input, applies an X gate to the input ket and returns the resulting ket
}

\examples{
 X(ket(1,0))
 X()
}
