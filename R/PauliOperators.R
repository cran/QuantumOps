#Generate every possible Pauli operator for n qubits
PauliOperators <- function(n,m=4^n,unique=TRUE){

	#P <- array( 0 , dim=c(2^n,2^n,m) )		#Each Pauli operator is 2^n by 2^n matrix, and there are 4^n different possible ones
	P <- list()

	# 1 = I , 2 = X , 3 = Y , 4 = Z
	p <- matrix( 0 , nrow=m,ncol=n)			#P is matrix where each row is quaternary number (base 4, one "digit" for each gate)

	if(unique){	#If all supplied Pauli Operators should be unqiue							
		idx <- 0:(4^n-1)					#There are 4^n different possible ones
		if(m < 4^n)							#If only want a subset of them
			idx <- sample(idx,size=m)
	}else{		#Or if we just want Pauli operators and don't care if some are the same (and can have more than all unique)
		idx <- sample(0:(4^n-1),replace=TRUE,size=m)	#choose any combination of them
	}					
	for(j in n:1){
		p[,j] <- floor( idx / ( 4^(j-1) ) ) %% 4 + 1
	}

	#List for the gates {I,X,Y,Z}
	gates <- list(I(),X(),Y(),Z())

	#For each different Pauli Operator possible
	for(j in 1:m){
		if(n > 1){
			P <- c( P , list( do.call(tensor, gates[p[j,]] ) ))	#Get list of Pauli gates, tensor them together, then convert to list, then concatenate to P
		} else{
			P <- c( P , gates[p[j,]] )	#If only for 1 qubit, don't call tensor function, just add directly
		}
	}
	P
}
