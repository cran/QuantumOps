\name{CYg}
\alias{CYg}
\title{CYg}
\usage{
CYg(ket)
}
\arguments{
  \item{ket}{ 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
ket after a Controlled-Y gate is applied
}

\description{
Applies a Controlled-Y gate to the input ket and return the resulting ket
}

\examples{
 CYg(ket(1,1,1,1))
}
