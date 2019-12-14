\name{FullAdder}
\alias{FullAdder}
\title{FullAdder}
\usage{
FullAdder(n=4,cin=0,a=1,b=2,cout=3)
}
\arguments{
  \item{n}{ Number of qubits in input quantum state  }
  \item{cin}{ index of the carry in (qu)bit  }
  \item{a}{ Index of the first operand (qu)bit  }
  \item{b }{ Index of the second operand (qu)bit  }
  \item{cout }{ Index where the output carry (qu)bit will be places  }
}

\value{
 A list with elements containing the quantum operations (matrices) for the full adder in each cycle.
}

\description{
 Provides the quantum operatios for a full-adder with the specified input and output indices. Uses the circuit developed by Cheng and Tseng <DOI:10.1049/el:20020949>. Uses CNOT and TOFFOLI gates, with the TOFFOLI gates being broken down into H, T, and CNOT gates. The SUM (qu)bit gets places where the b operand (qu)bit is.
}

\examples{
 FullAdder(n=4,cin=0,a=1,b=2,cout=3)
}
