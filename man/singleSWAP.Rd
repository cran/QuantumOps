\name{singleSWAP}
\alias{singleSWAP}
\title{singleSWAP}
\usage{
singleSWAP(nQubits=2,a=0,b=1,...)
}
\arguments{
  \item{nQubits}{ Number of qubits in target ket}

  \item{a}{Index of first qubit to swap, indexed from 0}

  \item{b}{Index of second qubit to swap, indexed from 0}

  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matrix of the singleSWAP gate or ket after an singleSWAP gate is applied
}

\description{
Implements the SWAP gate between two qubits, which can be in a larger ket. If no argument is supplied, returns the matrix of the gate. If ket given as input, applies the gate to the input ket and returns the resulting ket. In its default configuration it is the same as standard SWAP.
}

\examples{
 singleSWAP(2,0,1, ket(1,2,3,4) )
 singleSWAP(4,0,3, intket( c(1,5),4,c(1,2)) )
}
