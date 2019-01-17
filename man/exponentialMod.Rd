\name{exponentialMod}
\alias{exponentialMod}
\title{exponentialMod}
\usage{
exponentialMod(a,N)
}
\arguments{	
  \item{a}{random number that is used as input to Shor's algorithm}

  \item{N}{Number that Shor's algorithm is to factor}
}

\value{
A function that takes argument x and returns a^x modulus N
}

\description{
Creates a function that raises a number to a power modulus another number. Is a fix for information loss due to extremely large numbers. It takes the modulus for every multiplication
}

\examples{
 exponentialMod(8,21)
 exponentialMod(2,15)
}
