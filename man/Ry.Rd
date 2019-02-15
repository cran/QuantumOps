\name{Ry}
\alias{Ry}
\title{Ry}
\usage{
Ry(theta,...)
}
\arguments{
  \item{theta}{Radians to phase rotate the ket around the y-axis}

  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the Rz gate or ket after an Rz gate is applied
}

\description{
If no second argument is supplied, returns the matrix of an Ry rotation gate of the specified radians. If ket given as second argument, applies the Ry gate to the input ket and returns the resulting ket. 
}

\examples{
 Ry(pi,ket(1,0))
 Ry(pi)
}
