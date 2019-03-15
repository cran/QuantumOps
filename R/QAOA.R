#' @export
QAOA <- function(clauses,p=1,gamma=pi/p,beta=pi/(2*p),displayProgress=FALSE){

	#Get qubit count and set up amplitudes for initial state (ansatz)
	n <- dim(clauses)[2]					#Number of qubits = # columns in clause (1 qubit per bit)
	N <- 2^(n+1)							#Set up amplitudes (one ancilla qubit)
	v <- do.call(ket,as.list(rep(1,N)))

	m <- dim(clauses)[1]		#Number of clauses

	#Classically check each case
	if(displayProgress){
		color <- checkCases(clauses,colorCode=TRUE)
		lna <- c("<20 %","> 20%","> 40%","> 60%","> 80%","> 90%","100%")
		lc <- c("Violet","Purple","Blue","Green","Yellow","Orange","Red")
	}

	for(P in 1:p){
		#Apply U(C,g)	controlled phase operations
		Rg <- Rz(gamma)		#Create phase gate matrix
		#For each clause
		for(c in 1:m){
			clause <- clauses[c,]
	
			zeros <- which(clause == 0)	- 1			#Get indices that are 0 in clause (indexed from 0 instead of 1)
			if(length(zeros) > 0)
				for(j in 1:length(zeros))
					v <- single( X(), n+1, zeros[j], v )	#Apply X gates to qubits for those indices

			cQubits <- which( clause==0 | clause==1 ) - 1	#Get indices that are 0 or 1 (indexed from 0 instead of 1)
			cQubits <- c(cQubits,n)							#Add ancilla qubit (at index n) to array
			#some processing for R
			gate <- list(Rg)		#list version of gate
			ln <- list(n+1)			#list version of # of qubits
			arg <- c( gate , ln, as.list(cQubits) )				#list containing gate matrix, control qubits, target qubit
			g <- do.call( cntrld, arg )							#call cntrld function with arguments to get matrix

			v <- U( g , v)		#Apply gate
	
			if(length(zeros) > 0)
				for(j in 1:length(zeros))
					v <- single( X(), n+1, zeros[j], v )	#Reapply X gates to qubits for zero indices
			
		}

		#Apply U(B,b)	x rotations
		v <- many( Rx(2*beta) , n+1 , v )			#Apply X rotation to all qubits
		
		if(displayProgress){
			plotprobs(v,as.vector(rbind(color,color)),TRUE,lna,lc)	#Ancilla qubit does not count, so each case has 2 amplitudes
			Sys.sleep(1)
		}
	}
			
	v
}		
	


