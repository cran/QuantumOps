\name{U}
\alias{U}
\title{U}
\usage{
U(...)
}
\arguments{
  \item{...}{ List of quantum gates in matrix form, optionally the last argument is ket (column vector) that is input to the gate   }
}

\value{
Matix of the gate that is the tensor product of all input gates, or the ket which is the result of the gate applied to the input ket
}

\description{
Takes as input a list of gates (in matrix form) and creates the tensor product, forming a higher dimensional gate. If the last argument is a ket, the gate is applied to the ket and the ket returned. If last argument is another gate, it returns the tensor product of all gates
}

\examples{
 U(X(),ket(1,0))
 U(H(),H(),ket(1,0,0,0))
 U(I(),X(),ket(1,0,1,0))
 U(I(),X())
}
