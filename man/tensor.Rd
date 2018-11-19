\name{tensor}
\alias{tensor}
\title{tensor}
\usage{
tensor(k1,k2)
}
\arguments{
  \item{k1}{ ket (column vector) or gate (matrix) }

  \item{k2}{ ket (column vector) or gate (matrix)   }
}

\value{
The tensor product of \code{k1} and \code{k2}
}

\description{
If the inputs are n-dimensional kets, the output is a 2-n dimensional ket representing the combined system
}

\examples{
 tensor(ket(1,1,1,1),ket(1,0,0,1))
}
