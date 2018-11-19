\name{inner}
\alias{inner}
\title{inner}
\usage{
inner(w,v)
}
\arguments{
  \item{w}{ ket (column vector) that is the left side of the innter product, converted to a bra before the dot product   }
	
  \item{v}{ ket (column vector) that is the right side of the inner product}
}

\value{
Value of the inner product
}

\description{
Finds the inner product of two kets, <w|v>. w and v can be the same
}

\examples{
 inner(ket(1,0),ket(1,1))
}
