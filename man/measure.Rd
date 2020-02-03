\name{measure}
\alias{measure}
\title{measure}
\usage{
measure(...,l2r=FALSE)
}
\arguments{
  \item{...}{ The input ket to measure. Optionally followed by integers specifying which qubits of the ket to measure. Qubits indexed from 0 from right to left }

  \item{l2r}{ Boolean which specifices if indexing should be performed from left to right. Is FALSE by default to maintain backwards compatibility, however all other functions index from left to right.}
}

\value{
A list with the first item a column vector containing normalized amplitudes of the measured ket and the second item the integer value of the state which was measured. If a list of qubits to measure was specified as an argument, there is a 3rd item in the list which is a vector of the binary measured values.
}

\description{
Probabilistically measures the input ket. By default measures all qubits, but if a list of integers is supplied it will measure only those qubits. Returns a list containing the state of the ket after measurement along with integer value of the state that was measured. Additionally, returns a vector of the measured binary values, if a list of qubits to measure was specified. 
}

\examples{
 measure(ket(1,0),l2r=TRUE)
 measure(ket(1,2,2,1),0,l2r=TRUE)
 measure(ket(1,2,3,4,5,6,7,8),0,l2r=TRUE)
 measure(ket(1,2,3,4,5,6,7,8),0,1,l2r=TRUE)
 measure(ket(1,2,3,4,5,6,7,8),0,1,2,l2r=TRUE)
}
