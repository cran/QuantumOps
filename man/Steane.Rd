\name{Steane}
\alias{Steane}
\title{Steane}
\usage{
Steane(v)
}
\arguments{
  \item{v}{ Single qubit ket to Steane encode }
}

\value{
Steane encoded ket containing 7 qubits
}

\description{
Takes an unencoded single qubit ket and converts it to a 7-qubit Steane encoded ket
}

\examples{
 Steane(ket(1,0))
 Steane(ket(0,1))
 Steane(ket(1,1))
}
