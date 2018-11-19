\name{hermitian}
\alias{hermitian}
\title{hermitian}
\usage{
hermitian(m)
}
\arguments{
  \item{m}{  gate operation (gate) that is to be checked   }
}

\value{
boolean indicating whether matrix is hermitian or not
}

\description{
Determines whether an operation (matrix) is hermitian by comparing it to its adjoint
}

\examples{
 hermitian(mm(0,1,1,0))
}
