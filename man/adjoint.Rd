\name{adjoint}
\alias{adjoint}
\title{adjoint}
\usage{
adjoint(x)
}
\arguments{
  \item{x}{ A ket (column vector), bra (row vecor), or gate (matrix)  }
}

\value{
The adjoint of \code{x}
}

\description{
Finds the adjoint of the input. An input ket will become a bra and input bra will become a ket
}

\examples{
 adjoint(ket(1,5))
}
