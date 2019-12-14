\name{RandomizeCompile}
\alias{RandomizeCompile}
\title{RandomizeCompile}
\usage{
RandomizeCompile(	C=rep( list(
				repeatTensor(I(),log( dim(G[[1]])[1],base=2))),
						length(G)+1)
				,G,combine=TRUE)
}
\arguments{
  \item{ C }{ List of easy cycles }
  \item{ G }{ List of hard cycles }
  \item{ combine }{ Boolean specifying if the output should be combined into one list or left seperate }
}

\value{
 If combine is TRUE, a list of cycles that are now Randomly Compiled. If combine is FALSE, a list of two lists, the first being the Randomly Compiled easy cycles and the second the hard cycles.
}

\description{
 Implements Randomized Compiling as described by Wallman and Emerson <DOI:10.1103/PhysRevA.94.052325>. Takes as input a list of easy cycles and a list of hard cycles. In this context, a cycle is the application of one operation to a register of qubits. Inserts randomizing Pauli gates after easy cycles, and corrective operations before the next easy cycle. The randomizations are then combined with the easy cycles. The first and last cycles are easy, with all other cycles alternating betweene asy and hard. Hence, the number of easy cycles should be one more than the number of hard cycles. Easy cycles (C) can be left unset, in which case Idle cycles will be inserted to enabled the randomizations.
}

\examples{
 RandomizeCompile( G=list( CX(), CX()))
 RandomizeCompile( G=list( controlled(gate=Z(),n=3,cQubits=0,tQubit=1) , 
			single(gate=H(),n=3,t=1) ))
}
