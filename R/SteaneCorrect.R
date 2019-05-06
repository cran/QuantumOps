
#' @export
SteaneCorrect <- function(v){
	if(length(v) != 2^7)
		print("SteanCorrect: Input is not a 7-qubit ket")
	#The 6 stabilizers for steane code
	g <- list(
		list(I(),I(),I(),X(),X(),X(),X()),
		list(I(),X(),X(),I(),I(),X(),X()),
		list(X(),I(),X(),I(),X(),I(),X()),
		
		list(I(),I(),I(),Z(),Z(),Z(),Z()),
		list(I(),Z(),Z(),I(),I(),Z(),Z()),
		list(Z(),I(),Z(),I(),Z(),I(),Z())
	)
	#Z syndrome
	ancQubits <- intket(0,3)	#3 ancilla qubits
	v <- tensor(v,ancQubits)	#add ancilla qubits to end of ket
	ancIdx <- 7:9				#indices of ancilla qubits
	for(j in 1:3)
		v <- single(H(),n=10,t=ancIdx[j],v)	#Apply hadamard to each ancilla qubit
	for(j in 1:3){				#Each ancilla conditionally applies each stabilizer
		for(k in 1:7){			#For each gate in the stabilizer, use ancilla as control
			if( !all( g[[j]][[k]] == I() ) )	#Don't need to apply I gates
				v <- U( cntrld(gate=g[[j]][[k]],n=10,ancIdx[j],k-1) , v)	#and 1 qubit as target
		}
	}	
	for(j in 1:3)
		v <- single(H(),n=10,t=ancIdx[j],v)	#Apply hadamard to each ancilla qubit
	m <- reduceMeasure(v,7,8,9,l2r=TRUE)	#Measure the ancilla qubits
	v <- m[[1]]								#Get the original ket back
	s <- m[[3]]								#Classical measure values, the syndrome
	Zidx <- convert_bin2dec(s)	#Get index for corrective Z gate
	if( Zidx == 0 ){
		print("No Z errors")
	} else{
		print(paste("Corrective Z gate performed on qubit",Zidx-1))
		v <- single(Z(),n=7,t=Zidx-1, v)	#Perform corrective Z gate
	}
	#X syndrome
	ancQubits <- intket(0,3)	#3 ancilla qubits
	v <- tensor(v,ancQubits)	#add ancilla qubits to end of ket
	ancIdx <- 7:9				#indices of ancilla qubits
	for(j in 1:3)
		v <- single(H(),n=10,t=ancIdx[j],v)	#Apply hadamard to each ancilla qubit
	for(j in 1:3){				#Each ancilla conditionally applies each stabilizer
		for(k in 1:7){			#For each gate in the stabilizer, use ancilla as control
			if( !all( g[[j]][[k]] == I() ) )	#Don't need to apply I gates
				v <- U( cntrld(gate=g[[j+3]][[k]],n=10,ancIdx[j],k-1) , v)	#and 1 qubit as target
		}
	}	
	for(j in 1:3)
		v <- single(H(),n=10,t=ancIdx[j],v)	#Apply hadamard to each ancilla qubit
	m <- reduceMeasure(v,7,8,9,l2r=TRUE)	#Measure the ancilla qubits
	v <- m[[1]]								#Get the original ket back
	s <- m[[3]]								#Classical measure values, the syndrome
	Xidx <- convert_bin2dec(s)	#Get index for corrective X gate
	if( Xidx == 0 ){
		print("No X errors")
	} else{
		print(paste("Corrective X gate performed on qubit",Xidx-1))
		v <- single(X(),n=7,t=Xidx-1, v)	#Perform corrective X gate
	}	
	v
}





