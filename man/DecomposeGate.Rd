\name{DecomposeGate}
\alias{DecomposeGate}
\title{DecomposeGate}
\usage{
DecomposeGate(path,g,TwoQubit=FALSE,n=1,tQubit=0,cQubit=1,prec=10)
}
\arguments{
  \item{path}{String of path to folder containing gridsynth binary (not including gridsynths file name). R must have permission to read and write from this folder, and to execute the binary. }
  \item{g}{If a single number, this is the Z-rotation angle to approximate. If a vector of length 3, it is the alpha, beta, and gamma parameters as defined in Schuld 2018 <arXiv:1804.00633>.}
  \item{TwoQubit}{Boolean specifying whether this is a single or controlled 2-qubit gate  }
  \item{n}{The total number of qubits in the system. If TwoQubit is TRUE, the returned circuit will have n+1 qubits due to the requirement of an ancilla qubit.  }
  \item{tQubit}{ The target qubit. If a single qubit gate, the gate is applied to this qubit. If a 2-qubit gate, this is the target qubit.  }
  \item{cQubit}{ Control qubit if a 2-qubit gate. Value does not matter for single qubit gate. }
  \item{prec}{ The binary precision of the approximation, which is passed to the gridsynth binary. }
}

\value{
List of cycles which approximates the input gate.
}

\description{
Uses the gridsynth algorithm Sellinger 2012 <arXiv:1212.6253>, which is available at https://www.mathstat.dal.ca/~selinger/newsynth/, to decompose arbitrary gates to the Clifford+T set. For decomposition of controlled 2-qubit gates, circuits from Amy 2013 <DOI:10.1109/TCAD.2013.2244643> are also used. 
}

\examples{
\dontrun{
 DecomposeGate(path="./",g=pi/5,TwoQubit=TRUE,n=3,tQubit=0,cQubit=1,prec=3)
}
}
