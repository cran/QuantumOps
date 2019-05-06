
#' @export
compareQuantumState <- function(nQubits,a,b){
	a <- a + 1										#R indexes by 1
	b <- b + 1
	f <- function(x){								#Function that is implemented by quantum oracle
		bits <- as.integer(intToBits(x))
		if( all( bits[a] == bits[b] ) ){
			1										#output 1 if all bits (qubits) are the same
		} else{
			0										#0 otherwise
		}
	}
	Uf(fun=f,nQubits-1,1)
}
