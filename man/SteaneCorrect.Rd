\name{SteaneCorrect}
\alias{SteaneCorrect}
\title{SteaneCorrect}
\usage{
SteaneCorrect(v)
}
\arguments{
  \item{v}{ Steane encoded qubit ket }
}

\value{
Steane encoded ket after error correction has been performed
}

\description{
Performs Steane error correction on an encoded qubit. Useful explanation provided by Devitt <DOI:10.1088/0034-4885/76/7/076001>
}

\examples{
 \dontrun{
 SteaneCorrect(Steane(ket(1,0)))
 SteaneCorrect(Steane(ket(0,1)))
 SteaneCorrect(Steane(ket(1,1)))
 SteaneCorrect(single(X(),n=7,t=2,Steane(ket(1,0))))
 }
}
