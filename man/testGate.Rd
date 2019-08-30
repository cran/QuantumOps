\name{testGate}
\alias{testGate}
\title{testGate}
\usage{
testGate(g,inputs=0:(dim(g)[1]-1) )
}
\arguments{
  \item{g}{ Matrix that represents a quantum gate (operation) }

  \item{inputs}{ Vector of indices of computational basis states to test. Default is that all computational basis states will be tested }
}

\value{
None
}

\description{
Takes a given quantum gate and tests it with computational basis states as input. Can test a subset of possible inputs if specified, otherwise it tests all possible inputs. Useful for testing user defined gates.
}

\examples{
 testGate(CX())
 testGate(CX(),inputs=c(0,1))
}
