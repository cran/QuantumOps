\name{QAOA}
\alias{QAOA}
\title{QAOA}
\usage{
QAOA(clauses,p=1,gamma=pi/p,beta=pi/(2*p),displayProgress=FALSE,byCycle=FALSE)
}
\arguments{
  \item{clauses}{ Matrix that specifies the clauses. Each row is a clause. Each row must contain the same number of columns as qubits, the bit length of the clauses. 0 and 1 are values which are added to clause, ignored bits should be set to any other value. }

  \item{p}{ Number of iterations that algorithm will run. Each iteration applies U(C,g) and U(B,b)  }

  \item{gamma}{Angle for U(C,g), currently the same for all iterations. Should be between 0 and 2*pi}

  \item{beta}{Angle for U(B,b), currently the same for all iterations. Should be between 0 and pi}

  \item{displayProgress}{Boolean which specifies if progress should be shown. If TRUE, a bar plot is continually updated showing the amplitudes}

  \item{byCycle}{ Boolean which specifies if function should return the circuit. If TRUE, rather than performing the algorithm it will generate and return the equivalent circuit.}
}

\value{
Ket after algorithm is applied
}

\description{
Implements a clause-based version of Quantum Approximation Optimization Algorithm (Farhi, Goldstone, and Gutmann 2014) <arXiv:1411.4028>. Takes as input a set of clauses and performs Controlled-Phase and Rx gates to perform optimization. See "An Introduction to Quantum Optimization Approximation Algorithm" (Wang and Abdullah 2018) for explanation.
}

\examples{
 QAOA(rbind(c(0,0),c(0,1)))
}
