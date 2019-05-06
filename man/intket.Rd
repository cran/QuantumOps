\name{intket}
\alias{intket}
\title{intket}
\usage{
intket(x,n,amplitudes=rep(1,length(x)))
}
\arguments{
  \item{x}{ Integer, or vector of integers, specifying the integer encoded state(s) of the ket }

  \item{n}{ Integer specifying the number of qubits in the ket }

  \item{amplitudes}{ Integer, or vector of integers, specifying the amplitudes for corresponding basis in x. Must be same length as x. Only relative values matter as the ket will be normalized. Default is for all states to have same amplitude.}
}

\value{
Column vector containing normalized amplitudes of a ket
}

\description{
Returns a ket (column vector) that has the encoded value of the specified integers. Implements what is commonly known as basis encoding. Does not simulate the state creation.
}

\examples{
 intket(0,1)
 intket(3,2)
 intket(4,3)
 intket( c(0,1), 4)
 intket( c(0,2), 4 , c(1,2) )
}
