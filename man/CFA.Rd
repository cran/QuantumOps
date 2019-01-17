\name{CFA}
\alias{CFA}
\title{CFA}
\usage{
CFA(y,epsilon=1e-2)
}
\arguments{	
  \item{y}{Value that function attempts to find. Typically comes from measurement of Shor's algorithm }

  \item{epsilon}{Acceptable error between value and fraction}

}

\value{
Vector containing numerator and denominator of fraction
}

\description{
Performs the continued fractions algorithm to find a fraction close to input value
}

\examples{
 CFA(285/14)
}
