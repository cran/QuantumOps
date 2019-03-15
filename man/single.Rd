\name{single}
\alias{single}
\title{single}
\usage{
single(gate,n,t,...)
}
\arguments{
  \item{gate}{ Single qubit gate to apply }

  \item{n}{ Number of qubits that are in the target ket  }

  \item{t}{ Target qubit that the gate will be applied to, other qubits are unmodified. Indexed from 0.}

  \item{...}{Either no argument or a ket that the gate will be applied to}
}

\value{
The matrix representing the application of a single gate to one of the qubits in a ket or a ket after the gate has been applied
}

\description{
Takes as input a gate and generates the matrix for that gate being applied to a single qubit in a ket by creating a tensor product of the matrix with Identity matrices. If a ket is supplied, the matrix will be applied to the ket
}

\examples{
 single(H(),4,1)
 single(H(),2,1,ket(1,0,0,0))
 single(X(),2,0,ket(1,0,0,0))
}
