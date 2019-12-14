\name{TOFFOLI}
\alias{TOFFOLI}
\title{TOFFOLI}
\usage{
TOFFOLI(...,byCycle=FALSE,n=3,cQubits=c(0,1),tQubit=2)
}
\arguments{
  \item{...}{ No argument, or an 8 dimensional (3 qubit) ket (column vector) that is input to the gate   }
  \item{byCycle}{Boolean specifying if the circuit should be generated. If TRUE, rather than returning a matrix or performing a TOFFOLI gate, it returns a list of cycles of standard gates which implements the TOFFOLI gate.}
  \item{n}{Number of qubits in the system.}
  \item{cQubits}{Vector of control qubit indices. Indexed from 0.}
  \item{tQubit}{Index of target qubit. Indexed from 0.}
}

\value{
Matix of the TOFFOLI gate or ket after a TOFFOLI gate is applied. If byCycle is TRUE, a list of cycles implementing the TOFFOLI.
}

\description{
If no argument is supplied, returns the matrix of TOFFOLI gate. If ket given as input, applies a TOFFOLI gate to the input ket and returns the resulting ket. If byCycle is TRUE, it generates the cycles which implement the TOFFOLI gate with standard gates.
}

\examples{
 TOFFOLI(ket(1,1,1,1,0,1,0,1))
 TOFFOLI()
}
