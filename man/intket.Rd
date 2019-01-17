\name{intket}
\alias{intket}
\title{intket}
\usage{
intket(x,n)
}
\arguments{
  \item{x}{ Integer specifying the encoded value of the ket }

  \item{n}{ Integer specifying the number of qubits in the ket }
}

\value{
Column vector containing normalized amplitudes of a ket
}

\description{
Returns a ket (column vector) that has the encoded value of the specified integer
}

\examples{
 intket(0,1)
 intket(3,2)
 intket(4,3)
}
