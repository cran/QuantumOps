\name{controlled}
\alias{controlled}
\title{controlled}
\usage{
controlled(gate,n,cQubits,tQubit)
}
\arguments{
  \item{gate}{ single qubit gate to create controlled version of   }

  \item{n}{ Number of qubits in the ket, including target, control, and all other qubits}

  \item{cQubits}{Vector of qubit indices. There can be between 1 and n-1 control qubits, where n is the number of qubits in the ket. Qubits are indexed from 0, starting at the most significant qubit}

  \item{tQubit}{Indiced of the target qubit. Qubits are indexed from 0, starting at the most significant qubit}
}

\value{
A matrix representing the operation of a controlled qubit gate on any subset of the input ket
}

\description{
Creates a matrix representing a controlled gate on a system of qubits. The target and control qubits can be located at arbitrarying positions.
}

\examples{
 controlled(X(),n=2,cQubits=0,tQubit=1)
 controlled(X(),n=4,cQubits=c(0,1,2),tQubit=3)
}
