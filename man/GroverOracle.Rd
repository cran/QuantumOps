\name{GroverOracle}
\alias{GroverOracle}
\title{GroverOracle}
\usage{
GroverOracle(w,input)
}
\arguments{
  \item{w}{Integer specifying the state to search for, between 0 and 2^n-1 where n is the number of qubits}

  \item{input}{ Either integer specifying size of operation (in number of qubits it is applied to) or input ket to apply GroverOracle to   }
}

\value{
Either the matrix of the GroverOracle gate of the specified size or ket after a GroverOracle operation is applied
}

\description{
If integer is input, returns the matrix of GroverOracle operation on the integer number of qubits. If ket given as input, applies a GroverOracle operation to the input ket and returns the resulting ket
}

\examples{
 GroverOracle(0,ket(1,1,1,1,1,1,1,1))
 GroverOracle(0,3)
}
