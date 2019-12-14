\name{SynthesizeCircuit}
\alias{SynthesizeCircuit}
\title{SynthesizeCircuit}
\usage{
SynthesizeCircuit(l)
}
\arguments{
  \item{ l }{ list containing the quantum operators of each cycle. The quantum operators should be unitary matrices which act on a number of qubits. Each entry in l should be of the same dimension. Optionally, l can be a of such lists, in which case each list will be interleaved. }
}

\value{
 A matrix representing the result of applying each operation listed in l
}

\description{
 Converts the list form of a quantum circuit into a matrix representation. If the input is a single list, this function multiplies each entry. If each entry is a 4x4 unitary matrix, this function will multiply all, starting with the first, and generate a single 4x4 unitary matrix. If the input is a list of lists, this function will perform the same operation but interleave each list. The lists can be of different lengths.
}

\examples{
\dontrun{
 SynthesizeCircuit( list( tensor(X(),X()) , tensor(Y(),X()),
		 tensor(I(),X()) , tensor(Z(),Z())  ))
}
\dontrun{
 SynthesizeCircuit( list(  list(  tensor(X(),X()) , tensor(I(),X()) ),
		 list( tensor(Y(),X()) , tensor(Z(),Z()) ) ) )
}
}
