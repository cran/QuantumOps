\name{PhaseDamping}
\alias{PhaseDamping}
\title{PhaseDamping}
\usage{
PhaseDamping(p,Ppd)
}
\arguments{
  \item{p}{ Input quantum state, in density matrix format  }
  \item{Ppd}{The probability of phase Damping. }
}

\value{
The quantum state, after Phase Damping has been applied.
}

\description{
Implements Phase Damping noise model on the input quantum state. Formulas taken from <DOI:10.1103/PhysRevA.90.062320>.
}

\examples{
 PhaseDamping(p=convert_ket2DM(ket(1,0)),Ppd=0.01)
}
