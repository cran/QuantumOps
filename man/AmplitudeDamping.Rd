\name{AmplitudeDamping}
\alias{AmplitudeDamping}
\title{AmplitudeDamping}
\usage{
AmplitudeDamping(p,Pad)
}
\arguments{
  \item{p}{ Input quantum state, in density matrix format  }
  \item{Pad}{The probability of Amplitude Damping. Commonly referred to as gamma in the literature. }
}

\value{
The quantum state, after Amplitude Damping has been applied.
}

\description{
Implements Amplitude Damping noise model on the input quantum state. Formulas taken from <DOI:10.1103/PhysRevA.90.062320>.
}

\examples{
 AmplitudeDamping(p=convert_ket2DM(ket(1,0)),Pad=0.01)
}
