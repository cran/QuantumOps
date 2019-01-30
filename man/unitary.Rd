\name{unitary}
\alias{unitary}
\title{unitary}
\usage{
unitary(m,epsilon=1e-13)
}
\arguments{
  \item{m}{  gate operation (gate) that is to be checked   }

  \item{epsilon}{ Amount of error to tolerate. Accounts for numerical precision on practical computing systems}
}

\value{
boolean indicating whether matrix is unitary or not
}

\description{
Determines whether an operation (matrix) is unitary by comparing its inverse to its adjoint
}

\examples{
 unitary(mm(0,1,1,0))
 unitary(mm(0,1,1,0),1e-15)
}
