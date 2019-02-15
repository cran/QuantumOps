#' @export
GroverOracle <- function(w,input){
	i <- complex(1,0,1)
	if(length(input) == 1){				#input is constant specifying number of Qubits
		n <- input						#(inconsistent w/ QFT)
		N <- 2^n						#get dimension from qubit number
		GOm <- diag(N)					#Build Grover Oracle matrix
		GOm[w+1,w+1] <- -1				#Diagonal with -1 at w (which is state to identify)
		GOm								#return gate
	} else{								#input is ket to apply Grover Oracle to
		N <- length(input)				#get dimension from ket
		n <- log(N,base=2)				#extract # of qubits from dimension
		GOm <- diag(N)					#Build Grover Oracle matrix
		GOm[w+1,w+1] <- -1					#Diagonal with -1 at w (which is state to identify)
		GOm %*% input					#return ket after gate applied
	}	
}
