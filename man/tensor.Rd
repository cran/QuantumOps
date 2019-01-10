\name{tensor}
\alias{tensor}
\title{tensor}
\usage{
tensor(...)
}
\arguments{
  \item{...}{ kets (column vectosr) or gates (matrices) to take tensor product of }
}

\value{
The tensor product of all supplied arguments
}

\description{
Takes all arguments and combines them as a tensor product. Can be used to create a unified vector that represents multiple qubits or to create higher dimensional gates. If the inputs are two n-dimensional kets, the output is a 2-n dimensional ket representing the combined system. 
}

\examples{
 tensor(ket(1,0),ket(0,1),ket(1,0),ket(1,0))
 tensor(ket(1,1,1,1),ket(1,0,0,1))
 tensor(X(),I())
 tensor(H(),H(),H())
}
