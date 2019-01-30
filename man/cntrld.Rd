\name{cntrld}
\alias{cntrld}
\title{cntrld}
\usage{
cntrld(gate,n,...)
}
\arguments{
  \item{gate}{ single qubit gate to create controlled version of   }

  \item{n}{ Number of qubits in the ket, including target, control, and all other qubits}

  \item{...}{List of qubits. The last qubit in the list is the target. Any others listed before it are control qubits. Can be any number between 1 and n-1 control qubits, where n is the number of qubits in the ket. Qubits are indexed from 0, starting at the most significant qubit}
}

\value{
A matrix representing the operation of a controlled qubit gate on any subset of the input ket
}

\description{
Creates a matrix representing a controlled gate on a system of qubits. The target and control qubits can be located at arbitrarying positions.
}

\examples{
 cntrld(X(),2,0,1)
 cntrld(X(),2,1,0)
 cntrld(Y(),4,2,3)
 cntrld(X(),8,0,5)
}
