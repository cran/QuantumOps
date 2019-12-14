\name{nBitAddition}
\alias{nBitAddition}
\title{nBitAddition}
\usage{
nBitAddition(n)
}
\arguments{
  \item{n}{Length of input (qu)bit strings. }
}

\value{
 A list containing the quantum circuit (each elemented is one cycle of the circuit) performing n-bit addition. 
}

\description{
 Strings together output from FullAdder function to create multi-(qu)bit addition. It assumes the input operands are laid out as in <DOI:10.1049/el:20020949>. From left to right (top to bottom) the order is C0, a1, b1, C1, a2, b2, C2, .... bn-1, Cn. There must be 3n+1 qubits in order to perform n-(qu)bit addition.
}

\examples{
\dontrun{
 nBitAddition(2)
}
}
