\name{G}
\alias{G}
\title{G}
\usage{
G(a,b,g,p=0,...)
}
\arguments{
  \item{a}{First angle}
  \item{b}{second angle}
  \item{g}{third angle}
  \item{p}{global phase}
  \item{...}{ No argument, or ket (column vector) that is input to the gate   }
}

\value{
Matrix of the G gate or ket after an G gate is applied
}

\description{
Creates quantum gate defined by 4 angles as demonstrated by Barenco (1995). If no argument is supplied, returns the matrix of G gate. If ket given as input, applies an G gate to the input ket and returns the resulting ket
}

\examples{
 G(0,0,0,0,ket(1,0))
 G(1,1,1)
}
