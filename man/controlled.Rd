\name{controlled}
\alias{controlled}
\title{controlled}
\usage{
controlled(gate,nCQubits=1)
}
\arguments{
  \item{gate}{ Matrix representation of a quantum gate   }

  \item{nCQubits}{Integer that specifies how many control qubits the gate should have. Default of 1}
}

\value{
Matrix representing a controlled version of the input gate
}

\description{
Returns a matrix representation of a quantum gate which is a controlled version of the input.
}

\examples{
 controlled(mm(0,1,1,0))
 controlled(X())
 controlled(X(),2)
}
