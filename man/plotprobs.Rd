\name{plotprobs}
\alias{plotprobs}
\title{plotprobs}
\usage{
plotprobs(v,color=rep("Blue",length(v)),customLegend=FALSE,lgNm="",lgCl="")
}
\arguments{
  \item{v}{ ket that is to be plotted }

  \item{color}{ Text, possibly an array, specifying the colors of the bars  }

  \item{customLegend}{Boolean specifying if a custom legend should be inserted}

  \item{lgNm}{Vector of legend names}

  \item{lgCl}{Vector of legend colors}
}

\value{
A plot
}

\description{
Plots the probabilities of each of the amplitudes of ket in a barplot
}

\examples{
 plotprobs(ket(1,0,1,0),color=c("Red","Blue","Red","Blue"))
}
