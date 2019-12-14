#Equivalent to CCNOT
#' @export
TOFFOLI <- function(...,byCycle=FALSE,n=3,cQubits=c(0,1),tQubit=2){
	if(!byCycle){	#If just want the matrix, or to apply it to ket
		TOFFOLI <- matrix(c(1,0,0,0,0,0,0,0,
						0,1,0,0,0,0,0,0,
						0,0,1,0,0,0,0,0,
						0,0,0,1,0,0,0,0,
						0,0,0,0,1,0,0,0,
						0,0,0,0,0,1,0,0,
						0,0,0,0,0,0,0,1,
						0,0,0,0,0,0,1,0),nrow=8,ncol=8)
		ket <- list(...)
		if(length(ket) == 0){
			TOFFOLI
		} else {
			TOFFOLI %*% ket[[1]]
		}
	}else{		#Want a list by cycle, provide a decomposed version in CNOT, T, and H gates
		if(length(cQubits) > 2)
			print("ERROR - TOFFOLI does not have 2 inputs")
		c1 <- cQubits[1]
		c2 <- cQubits[2]
		t <- tQubit
		g <- list()
		g <- c(g,list(	single(H(),n=n,t=t) ))						#Hadamard on target
		g <- c(g,list(	controlled(X(),n=n,cQubits=c2,tQubit=t) ))	#CNot from c2 to target
		g <- c(g,list(	single(adjoint(T()),n=n,t=t) ))				#adjoint of T on target
		g <- c(g,list(	controlled(X(),n=n,cQubits=c1,tQubit=t) ))	#CNot from c1 to target
		g <- c(g,list(	single(T(),n=n,t=t) ))						#T on target
		g <- c(g,list(	controlled(X(),n=n,cQubits=c2,tQubit=t) ))	#CNot from c2 to target
		g <- c(g,list(	single(adjoint(T()),n=n,t=t) ))				#adjoint of T on target
		g <- c(g,list(	controlled(X(),n=n,cQubits=c1,tQubit=t) ))	#CNot from c1 to target
		##Two part
		g1 <- single(T(),n=n,t=c2)	#T on c2
		g2 <- single(T(),n=n,t=t)	#T on target
		g <- c(g,list(	g2 %*% g1 ))
		##Two part
		g1 <- controlled(X(),n=n,cQubits=c1,tQubit=c2)	#CNot from c1 to c2
		g2 <- single(H(),n=n,t=t)						#H on target
		g <- c(g,list(	g2 %*% g1 ))
		##Two part
		g1 <- single(T(),n=n,t=c1)						#T on c1
		g2 <- single(adjoint(T()),n=n,t=c2)				#adjoint T on c2
		g <- c(g,list(	g2 %*% g1 ))
		g <- c(g,list(	controlled(X(),n=n,cQubits=c1,tQubit=c2) ))
		g
	}
}
