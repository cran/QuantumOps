\name{QFT}
\alias{QFT}
\title{QFT}
\usage{
QFT(input)
}
\arguments{
  \item{input}{ Either integer specifying size of operation (in number of qubits it is applied to) or input ket to apply QFT to   }
}

\value{
Either the matrix of the QFT gate of the specified size or ket after a QFT operation is applied
}

\description{
If integer is input, returns the matrix of QFT operation on the integer number of qubits. If ket given as input, applies a QFT operation to the input ket and returns the resulting ket
}

\examples{
 QFT(ket(1,0))
 QFT(ket(1,0,0,1))
 QFT(3)
}
