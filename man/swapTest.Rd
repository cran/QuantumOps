\name{swapTest}
\alias{swapTest}
\title{swapTest}
\usage{
swapTest(v,a,b)
}
\arguments{
  \item{v}{ Ket (column vector) that swap test is applied to. Should be a tensor product of two quantum state. }

  \item{a}{ Vector of indices of |a> within v }

  \item{b}{ Vector of indices of |b> within v}
}

\value{
Ket which contains the modified input ket, v, along with a leading ancillary qubit. 
}

\description{
Encodes absolute square of inner product of two states, |<a|b>|^2, into an ancilliary qubit. It swaps the states of |a> and |b> conditioned on the ancilla which results in a state where the probability of measuring the ancilla qubit in the 0 state is equal to 1/2 - 1/2*(|<a|b>|^2). The ancilla qubit is inserted before qubit index 0, as the most significant qubit. Full explanation can be found in "Supervised Learning with Quantum Computers" <DOI:10.1007/978-3-319-96424-9>.
}

\examples{
 swapTest(intket(3,4),a=0:1,b=2:3)
 swapTest(intket(5,4),a=0:1,b=2:3)
}
