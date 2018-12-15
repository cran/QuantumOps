\name{H}
\alias{H}
\title{H}
\usage{
H(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the H gate or ket after a Hgate is applied
}

\description{
If no argument is supplied, returns the matrix of H gate. If ket given as input, applies an H gate to the input ket and returns the resulting ket
}

\examples{
 H(ket(1,0))
 H()
}
