\name{Rg}
\alias{Rg}
\title{Rg}
\usage{
Rg(theta,ket)
}
\arguments{
  \item{theta}{Radians to phase rotate the ket}

  \item{ket}{ ket (column vector) that is input to the gate   }
}

\value{
ket after an R gate is applied
}

\description{
Applies an R phase gate of the specified radians to the input ket and returns the resulting ket
}

\examples{
 Rg(pi,ket(1,0))
}
