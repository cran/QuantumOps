\name{BELLg}
\alias{BELLg}
\title{BELLg}
\usage{
BELLg(ket)
}
\arguments{
  \item{ket}{Ket that has dimension of 4 (2 qubits)  }
}

\value{
Four dimensional ket which is the bell gate \code{ket}
}

\description{
A Bell gate is a H gate on the first qubit followed by a CNOT, with the second qubit as the target
}

\examples{
 BELLg(ket(1,0,0,0))
}
