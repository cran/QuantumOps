#generate a normalized ket (column vector) with arbitrary no of states
#' @export
intket <- function(x,n,amplitudes=rep(1,length(x))){
		if(length(x) == 1 && x >= 2^n){
			print(paste("Warning:",n,"qubits are insufficient to represent encoded decimal",x))
		} else if( any(x >= 2^n) ){
			print(paste("Warning:",n,"qubits are insufficient to represent encoded decimal",x))
		}
		if( length(x) != length(amplitudes) )
			print(paste("Warning: Number of specified basis states (",length(x),") not equal to number of amplitudes (",length(amplitudes),")",sep=""))
		amp <- rep(0,2^n)
		amp[x+1] <- amplitudes
		k <- do.call(ket,as.list(amp))
		k
}
