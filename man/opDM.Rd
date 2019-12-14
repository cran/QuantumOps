\name{opDM}
\alias{opDM}
\title{opDM}
\usage{
opDM(V,G)
}
\arguments{
  \item{ V }{ Input density matrix }
  \item{ G }{ Quantum operation to apply to density matrix }
}

\value{
 A density matrix which has been modofied by the input quantum operation
}

\description{
 Applies a quantum operation to a density matrix
}

\examples{
 opDM( V=convert_ket2DM(ket(1,0)) , G=X()  )
}
