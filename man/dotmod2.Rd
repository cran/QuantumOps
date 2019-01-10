\name{dotmod2}
\alias{dotmod2}
\title{dotmod2}
\usage{
dotmod2(x,a)
}
\arguments{
  \item{x}{ integer }

  \item{a}{ integer  }
}

\value{
 Binary value resulting from the bit-wise dot product modulus 2
}

\description{
Takes two integers and takes the dot product of their binary representations. Output is the value of the dot product, modulus 2
}

\examples{
 dotmod2(5,5)
 dotmod2(1,2)
 dotmod2(0,1)
}
