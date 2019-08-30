\name{repeatTensor}
\alias{repeatTensor}
\title{repeatTensor}
\usage{
repeatTensor(g,n)
}
\arguments{
  \item{g}{ Object, typically a gate, that is to be tensored with itself   }

  \item{n}{ Number of times to tensor g with itself }
}

\value{
The input g tensored by itself n times
}

\description{
Repeatedly tensors the input with itself
}

\examples{
 repeatTensor(X(),2)
 repeatTensor(X(),2)
}
