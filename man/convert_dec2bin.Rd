\name{convert_dec2bin}
\alias{convert_dec2bin}
\title{convert_dec2bin}
\usage{
convert_dec2bin(x,len=32)
}
\arguments{
  \item{x}{ Integer }

  \item{len}{ Number of bits to represent integer with. Will crop most significant bits if insufficient length. }
}

\value{
 Vector of bits with MSB first
}

\description{
Takes an integer and returns an unsigned vector bits representing the same value
}

\examples{
 convert_dec2bin(10)
 convert_dec2bin(10,8)
}
