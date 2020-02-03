\name{convert_bin2dec}
\alias{convert_bin2dec}
\title{convert_bin2dec}
\usage{
convert_bin2dec(b)
}
\arguments{
  \item{b}{ Vector of bits with most significant bits first }
}

\value{
Integer value of bits
}

\description{
Takes a vector of unsigned bits with MSB first and produces integer value
}

\examples{
 convert_bin2dec( c(1,0,0) )

}
