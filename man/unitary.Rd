\name{unitary}
\alias{unitary}
\title{unitary}
\usage{
unitary(m)
}
\arguments{
  \item{m}{  gate operation (gate) that is to be checked   }
}

\value{
boolean indicating whether matrix is unitary or not
}

\description{
Determines whether an operation (matrix) is unitary by comparing its inverse to its adjoint
}

\examples{
 unitary(mm(0,1,1,0))
}
