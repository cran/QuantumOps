\name{R}
\alias{R}
\title{R}
\usage{
R(theta,...)
}
\arguments{
  \item{theta}{Radians to phase rotate the ket}

  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matix of the RZ gate or ket after an R gate is applied
}

\description{
If no second argument is supplied, returns the matrix of an R phase gate of the specified radians. If ket given as second argument, applies the R gate to the input ket and returns the resulting ket
}

\examples{
 R(pi,ket(1,0))
 R(pi)
}
