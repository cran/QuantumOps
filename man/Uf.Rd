\name{Uf}
\alias{Uf}
\title{Uf}
\usage{
Uf(fun,n,m)
}
\arguments{
  \item{fun}{ Function of an n-bit argument that produces an m-bit result}

  \item{n}{Integer that specifies the number of qubits in the data register, same as number of bits to function}

  \item{m}{Integer that specifies the number of qubits in the target register, same as number of bits as output of function}
}

\value{
Matrix of the operator (oracle) which implements the specified function)
}

\description{
Generates an operator (oracle) of specified size that implements the function that is passed to it. Assumes there are n qubits in data register and m qubits in target register.
}

\examples{
 Uf(function(x){x - floor(x/2)*2},1,1)
 Uf(function(x){0},2,2)
 Uf(function(x){1},2,2)
 Uf(function(x){x - floor(x/4)*4},2,2)
 Uf(function(x){x^3},3,4)
}
