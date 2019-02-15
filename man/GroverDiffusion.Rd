\name{GroverDiffusion}
\alias{GroverDiffusion}
\title{GroverDiffusion}
\usage{
GroverDiffusion(input)
}
\arguments{

  \item{input}{ Either integer specifying size of operation (in number of qubits it is applied to) or input ket to apply Grover Diffusion to   }
}

\value{
Either the matrix of the Grover Diffusion gate of the specified size or ket after a Grover Diffusion operation is applied
}

\description{
If integer is input, returns the matrix of Grover Diffusion operation on the integer number of qubits. If ket given as input, applies a Grover Diffusion operation to the input ket and returns the resulting ket
}

\examples{
 GroverDiffusion(ket(1,1,1,1,1,1,1,1))
 GroverDiffusion(3)
}
