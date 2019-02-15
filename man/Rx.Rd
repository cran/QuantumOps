\name{Rx}
\alias{Rx}
\title{Rx}
\usage{
Rx(theta,...)
}
\arguments{
  \item{theta}{Radians to phase rotate the ket around the x-axis}

  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the Rx gate or ket after an Rz gate is applied
}

\description{
If no second argument is supplied, returns the matrix of an Rx rotation gate of the specified radians. If ket given as second argument, applies the Rx gate to the input ket and returns the resulting ket. 
}

\examples{
 Rx(pi,ket(1,0))
 Rx(pi)
}
