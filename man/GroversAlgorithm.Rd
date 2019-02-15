\name{GroversAlgorithm}
\alias{GroversAlgorithm}
\title{GroversAlgorithm}
\usage{
GroversAlgorithm(n,w,iterations=n,printOutput=FALSE,plotOutput=FALSE,tag="")
}
\arguments{

  \item{n}{Number of qubits in the problem, not counting the extra ancillary qubit}

  \item{w}{Integer specifying the state to search for, between 0 and 2^n-1 where n is the number of qubits}

  \item{iterations}{ Number of iterations to apply the oracle and diffusion, optimal is approximately n }

  \item{printOutput}{Boolean specifying if the measurement probabilities should be printed as search progresses}

  \item{plotOutput}{Boolean specifying if the output probabilities should be plotted to a graph}

  \item{tag}{String which is attached to output file name if plotOutput is TRUE}
}

\value{
Ket after a Grover search has been applied to it
}

\description{
Applies Grover's search algorithm to a uniform ket to simulate a quantum search
}

\examples{
 GroversAlgorithm(7,0,14)
 GroversAlgorithm(7,0,14,printOutput=TRUE)
}
