\name{compareQuantumState}
\alias{compareQuantumState}
\title{compareQuantumState}
\usage{
compareQuantumState(nQubits,a,b)
}
\arguments{
  \item{nQubits}{ Number of qubits in a target ket. Should contain at least enough for states a and b and an additional last qubit. }

  \item{a}{ Vector of indices of first state to compare in a target ket }

  \item{b}{ Vector of indices of second state to compare in a target ket }
}

\value{
Matrix of the compareQuantumState oracle
}

\description{
Generates a matrix (quantum oracle) which will flip the last qubit in a quantum state if the qubits at indices in vectors a and b are the same
}

\examples{
 compareQuantumState(5,0:1,2:3)
}
