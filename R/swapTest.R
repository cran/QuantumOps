#Encode <a|b> into ancilla qubit
#' @export
swapTest <- function(v,a,b){		#v is input ket, a is vector of qubit indices of |a>, b is vector of qubit

	onecontrolled <- function(gate,nCQubits=1){
		d <- dim(gate)[1]		#side length of input gate
		D <- 2^(nCQubits)*d	#side length of controlled version
		g <- diag(1,D,D)		#create diagonal matrix
		g[ (D-d+1):D , (D-d+1):D ] <- gate	#set lower right (all control qubits = 1) to input gate
		g
	}



	if(length(a) != length(b))
		print("swapTest: Input states do not have same number of qubits")
	n <- log(length(v),base=2)		#number of qubits in original state

	v <- tensor( ket(1,1), v)		#Add ancilla qubit (in state |0> + |1>) to front of state
	for(j in 1:(length(a))){
		v <- U( onecontrolled( singleSWAP(n,a[j],b[j]), 1) , v)		#swap jth qubit of a and b conditioned on ancilla qubit
	}
	v <- single(H(),n+1,0,v)
	v
}

