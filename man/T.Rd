\name{T}
\alias{T}
\title{T}
\usage{
T(...)
}
\arguments{
  \item{...}{  No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the T gate or ket after an T gate is applied
}

\description{
If no argument is supplied, returns the matrix of T gate. If ket given as input, applies a T gate to the input ket and returns the resulting ket
}

\examples{
 T(ket(1,1))
 T()
}
