
#' @export
QAOA_maxcut <- function(connectionMatrix,p=1,gamma=pi/p,beta=pi/(2*p),displayProgress=FALSE){
	nQubits <- dim(connectionMatrix)[1]			#number of qubits
	clauses <- rbind( rep(-1,nQubits) )			#Null clause to start
	for(j in 1:nQubits){
		for(k in 1:nQubits){
			if(j != k){
				if(connectionMatrix[j,k] == 1){		#if edge from node j to node k
					clause <- rep(-1,nQubits)		#Make clause
					clause[j] <- 0					#source node in group 0
					clause[k] <- 1					#destination node in group 1
					clauses <- rbind(clauses,clause)	#Add to clauses
				}
			}
		}
	}
	QAOA(clauses[-1,],p,gamma,beta,displayProgress)		#Call QAOA w/ clauses (without first empty row)
}
