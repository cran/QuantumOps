\name{Shor}
\alias{Shor}
\title{Shor}
\usage{
Shor(N,trials=150,random=FALSE)
}
\arguments{	
  \item{N}{Number that Shor's algorithm is to factor}

  \item{trials}{Number of times to attempt before giving up}

  \item{random}{Boolean which determines whether seed is random or not}
}

\value{
Vector containing prime factors
}

\description{
 Implements Shor's algorithm by applying the quantum oracle, performing a QFT, measuring the output, and using continued fractions algorithm to find period. Period is then used with Euclidean algorithm to check if factors are legitimate prime factors. Is probabilistic and may fail. Factors 15 with ease and 21 occassionally. 
}

\examples{
 Shor(15,trials=2)
}
