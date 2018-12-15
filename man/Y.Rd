\name{Y}
\alias{Y}
\title{Y}
\usage{
Y(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matrix of the Y gate or ket after a Y gate is applied
}

\description{
If no argument is supplied, returns the matrix of the Y gate. If ket given as input, applies a Y gate to the input ket and return the resulting ket
}

\examples{
 Y(ket(1,0))
 Y()
}
