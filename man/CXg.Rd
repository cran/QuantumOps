\name{CXg}
\alias{CXg}
\title{CXg}
\usage{
CXg(ket)
}
\arguments{
  \item{ket}{ 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
ket after a Controlled-X gate is applied
}

\description{
Applies a Controlled-X gate to the input ket and return the resulting ket
}

\examples{
 CXg(ket(1,1,1,1))
}
