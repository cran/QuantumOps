\name{measure}
\alias{measure}
\title{measure}
\usage{
measure(...)
}
\arguments{
  \item{...}{ The input ket to measure. Optionally followed by integers specifying which qubits of the ket to measure. Qubits indexed from 0 from right to left }

}

\value{
A list with the first item a column vector containing normalized amplitudes of the measured ket and the second item the integer value of the state which was measured
}

\description{
Probabilistically measures the input ket. By default measures all qubits, but if a list of integers is supplied it will measure only those qubits. Returns the state of the ket after measurement along with integer value of the state that was measured.
}

\examples{
 measure(ket(1,0))
 measure(ket(1,2,2,1),0)
 measure(ket(1,2,3,4,5,6,7,8),0)
 measure(ket(1,2,3,4,5,6,7,8),0,1)
 measure(ket(1,2,3,4,5,6,7,8),0,1,2)
}
