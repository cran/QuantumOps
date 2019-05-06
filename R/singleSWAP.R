#' @export
singleSWAP <- function(nQubits=2,a=0,b=1,...){
	#Swap <- matrix(c(1,0,0,0, 0,0,1,0, 0,1,0,0, 0,0,0,1),nrow=4,ncol=4)
	N <- 2^nQubits

	swapBits <- function(x,a,b){		#take integer
		bin <- convert_dec2bin(x,N)		#convert to binary
		t <- bin[N-a]					#swap two bits
		bin[N-a] <- bin[N-b]
		bin[N-b] <- t
		convert_bin2dec(bin)			#convert back to integer
	}
	Swap <- matrix( rep(0,2^(2*nQubits)) , nrow=2^nQubits)	#blank matrix
	for(j in 1:(2^nQubits))
		Swap[swapBits(j-1,a,b)+1,j] <- 1

	ket <- list(...)
	if(length(ket) == 0){
		Swap
	} else {
		Swap %*% ket[[1]]	
	}
}
