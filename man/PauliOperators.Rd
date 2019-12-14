\name{PauliOperators}
\alias{PauliOperators}
\title{PauliOperators}
\usage{
PauliOperators(n,m=4^n,unique=TRUE)
}
\arguments{
  \item{ n }{ Size of the Pauli operators to generate, should be equal to the number of target qubits   }
  \item{ m }{ Number of different Pauli operators to generate }
  \item{ unique }{ Boolean indicating if each Pauli operator generated should be unqiue. Must be false is m > 4^n }
}

\value{
 A list of m Pauli operators of size n
}

\description{
 Generates random Pauli operators (tensor products of random I,X,Y,or Z gates applied to each qubit) that can be applied to register of n qubits. Used with Randomized Compiling, where random Pauli gates are applied to each qubit. 
}

\examples{
 PauliOperators( n=2,m=2,unique=FALSE)
}
