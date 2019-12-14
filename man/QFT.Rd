\name{QFT}
\alias{QFT}
\title{QFT}
\usage{
QFT(input,byCycle=FALSE,swaps=TRUE,CliffordT=FALSE,prec=10)
}
\arguments{
  \item{input}{ Either integer specifying size of operation (in number of qubits it is applied to) or input ket to apply QFT to   }
  \item{byCycle}{Boolean which specifies whether the circuit should be generated or not. If TRUE, rather than returning the matrix or performing the algorithm, the function will generate and return the equivalent circuit.}
  \item{swaps}{Boolean which specifies if the the SWAP gates required at the end of the QFT should be inserted. May not be necessary if qubit reordering is acceptable. Only valid if byCycle is TRUE.}
  \item{CliffordT}{Boolean which specifies if the generated circuit should be decomposed into the Clifford+T set. Only valid if byCycle is TRUE.}
  \item{prec}{The precision of the decomposition into the Clifford+T set. Only valid if byCycle and CliffordT are both TRUE.}
}

\value{
If the input is an integer, the matrix of the QFT gate of the specified size. If the input is a ket, the ket after a QFT operation is applied. If byCycle is TRUE, a list of the cycles of the algorithm.
}

\description{
If integer is input, returns the matrix of QFT operation on the integer number of qubits. If ket given as input, applies a QFT operation to the input ket and returns the resulting ket. If byCycle is TRUE, it generated the circuit for the QFT and returns a list of the cycles.
}

\examples{
 QFT(ket(1,0))
 QFT(ket(1,0,0,1))
 QFT(3)
}
