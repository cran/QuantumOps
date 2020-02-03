\name{CoherentNoise}
\alias{CoherentNoise}
\title{CoherentNoise}
\usage{
CoherentNoise(p,theta,randomRotation=FALSE)
}
\arguments{
  \item{ p }{ Input quantum state in density matrix format  }
  \item{ theta }{ Angle of rotation to apply  }
  \item{ randomRotation }{ Boolean specifying whether the rotation should be in a semi-random direction }
}

\value{
 The quantum state in density matrix format after noise has been applied
}

\description{
 Implements a model of coherent noise as used in <DOI:10.1038/s41534-018-0106-y>. It rotates each qubit around the z-axis by the specified amount. If randomRoation is set, it will rotate around the X, Y, or Z axis, which is chosen at random. Randomizing provides interesting side effects but is less representative of quantum noise.
}

\examples{
 CoherentNoise( p= convert_ket2DM(ket(1,1,1,1)),theta=0.06*pi) 
}
