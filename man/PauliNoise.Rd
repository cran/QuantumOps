\name{PauliNoise}
\alias{PauliNoise}
\title{PauliNoise}
\usage{
PauliNoise(p,e=ex+ey+ez,ex=e/3,ey=e/3,ez=e/3)
}
\arguments{
  \item{ p }{ Input quantum state, in density matrix format }
  \item{ e }{ Total amount of noise to apply the state, is the sum of ex, ey, and ez }
  \item{ ex }{ Amount of X noise to apply to the state }
  \item{ ey }{ Amount of Y noise to apply to the state }
  \item{ ez }{ Amount of Z noise to apply to the state }
}

\value{
 The quantum state in density matrix format, after Pauli noise has been applied to it
}

\description{
Applies stochastic Pauli noise to an input quantum state. If only e is set, it is equally distributed to X, Y, and Z error which is an isotropic Pauli noise model. Otherwise, levels can be set seperately for each.
}

\examples{
 PauliNoise( p=convert_ket2DM(ket(1,0)) , e=0.01  )
}
