\name{Rz}
\alias{Rz}
\title{Rz}
\usage{
Rz(theta,...)
}
\arguments{
  \item{theta}{Radians to phase rotate the ket around the z-axis}

  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the Rz gate or ket after an Rz gate is applied
}

\description{
If no second argument is supplied, returns the matrix of an Rz rotation gate of the specified radians. If ket given as second argument, applies the Rz gate to the input ket and returns the resulting ket. 
}

\examples{
 Rz(pi,ket(1,0))
 Rz(pi)
}
