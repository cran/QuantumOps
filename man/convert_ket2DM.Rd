\name{convert_ket2DM}
\alias{convert_ket2DM}
\title{convert_ket2DM}
\usage{
convert_ket2DM(v)
}
\arguments{
  \item{v}{ An input ket  }
}

\value{
	Density matrix representing same state as input ket
}

\description{
	Converts a ket (pure) description of quantum state and creates a density matrix representation of the same state. Density matrices can represent both pure and mixed states.
}

\examples{
 convert_ket2DM( ket(1,0) )
}
