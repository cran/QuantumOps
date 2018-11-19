\name{CZg}
\alias{CZg}
\title{CZg}
\usage{
CZg(ket)
}
\arguments{
  \item{ket}{ 4 dimensional (2 qubit) ket (column vector) that is input to the gate   }
}

\value{
ket after a Controlled-Z gate is applied
}

\description{
Applies a Controlled-Z gate to the input ket and return the resulting ket
}

\examples{
 CZg(ket(1,1,1,1))
}
