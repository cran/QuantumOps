\name{many}
\alias{many}
\title{many}
\usage{
many(gate,n,...)
}
\arguments{
  \item{gate}{ Single qubit gate to apply }

  \item{n}{ Number of qubits that the gate will be applied to  }

  \item{...}{Either no argument or a ket that the gates will be applied to}
}

\value{
The matrix representing the application of many gates or a ket after the gates have been applied
}

\description{
Takes as input a gate and generates the matrix for that gate being applied to multiple qubits by creating a tensor product of the matrix. If a ket is supplied, the matrix will be applied to the ket
}

\examples{
 many(H(),4)
 many(X(),2,ket(1,0,0,0))
}
