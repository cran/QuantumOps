\name{checkCases}
\alias{checkCases}
\title{checkCases}
\usage{
checkCases(clauses,colorCode=FALSE)
}
\arguments{
  \item{clauses}{ Matrix that specifies the clauses. Each row is a clause. Each row must contain the same number of columns as qubits, the bit length of the clauses. 0 and 1 are values which are added to clause, ignored bits should be set to any other value. }

  \item{colorCode}{Boolean which specifies if data should be returned as list of colors rather than numerical data}
}

\value{
Array of numbers or string color names
}

\description{
Takes in a matrix of clauses and determines what percentage of the clauses each possible value satisfies.
}

\examples{
 checkCases( rbind(c(1,-1),c(1,1) ) )
}
