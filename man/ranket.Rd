\name{ranket}
\alias{ranket}
\title{ranket}
\usage{
ranket(n)
}
\arguments{
  \item{n}{Number of qubits in generated get }
}

\value{
A ket with a randomized state
}

\description{
Generates a random ket by selecting random polar coordinates (theta,phi) for each. Approach taken from <DOI:10.1103/PhysRevA.95.062338>.
}

\examples{
 ranket(4)
}
