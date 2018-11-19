\name{controlled}
\alias{controlled}
\title{controlled}
\usage{
controlled(gate)
}
\arguments{
  \item{gate}{ Matrix representation of a quantum gate   }
}

\value{
Matrix representing a controlled version of the input gate
}

\description{
Returns a matrix representation of a quantum gate which is a controlled version of the input.
}

\examples{
 controlled(mm(0,1,1,0))
}
