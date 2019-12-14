

#Convert a circuit into a randomly compiled circuit
RandomizeCompile <- function(	C=rep( list(
				repeatTensor(I(),log( dim(G[[1]])[1],base=2))),
						length(G)+1)
				,G,combine=TRUE){
	
	#Index for easy gates
	# 1 = First easy gate followed by twirling gates
	#Index for hard gates
	# 1 = First hard gate following the first easy gates

	n <- log( dim(C[[1]])[1],base=2)	#n qubits is log2 of rows of gates

	#K easy cycles, K-1 hard cycles (one easy cycle again at the end)
	K <- length(C)
	if( length(G) != K-1)
		pp("Error: RandomizeCompile - Easy (C) and Hard gate set (G) have length mismatch length(C) = ",length(C)," length(G) = ",length(G),"and should be length(G) = length(C)-1")


	#Random Pauli Operator,s
	T <- PauliOperators(n=n,m=K-1,unique=FALSE)	#Twirling set (unique false because we can reuse pauli twirling gates)
	Tc <- T				#Correction Twirling set (will be overwritten)

	#For first cycle
	C[[1]] <- T[[1]] %*% C[[1]] 	#First easy gate followed by first twirling gates
	#First correction is adjoint of first twirling gates commuted through first hard gate
	Tc[[1]] <- G[[1]] %*% adjoint(T[[1]]) %*% adjoint(G[[1]])

	#For each easy/hard cycle that is not first or last
	if(K > 2){
	for(k in 2:(K-1)){
		#Twirling correction is adjoint of twirling for that cycle commuted through hard gate
		Tc[[k]] <- G[[k]] %*% adjoint(T[[k]]) %*% adjoint(G[[k]])

		#Create dressed cycle by applying corrective twirling for previous, easy gates, then new twirling
		C[[k]] <- T[[k]] %*% C[[k]] %*% Tc[[k-1]]	
	}
	}

	#For last cycle
	C[[K]] <-  C[[K]] %*% Tc[[K-1]]			#Do not twirl output


	#Return the list
	if(combine){			#Interleave easy (C) and hard (G) gates so it's just one list
		idx <- order(c(seq_along(C), seq_along(G)))		#Thanks to Arun on Stack Exchange!
		(c(C,G))[idx]
	} else{					#Leave them seperate lists, make a list of lists
		list(C,G)
	}
}








