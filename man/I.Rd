\name{I}
\alias{I}
\title{I}
\usage{
I(...)
}
\arguments{
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the I gate or ket after an I gate is applied
}

\description{
If no argument is supplied, returns the matrix of I gate. If ket given as input, applies an I gate to the input ket and returns the resulting ket
}

\examples{
 I(ket(1,0))
 I()
}
