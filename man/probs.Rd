\name{probs}
\alias{probs}
\title{probs}
\usage{
probs(ket)
}
\arguments{
  \item{ket}{  ket (column vector) that is input to the gate   }
}

\value{
Column vector containing probabilities
}

\description{
Returns a column vector containing the probabilities of measuring the system in each state
}

\examples{
 probs(ket(1,1))
}
