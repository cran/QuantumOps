#' @export
QFT <- function(input){
	i <- complex(1,0,1)
	if(length(input) == 1){				#input is constant specifying dimension (in # of qubits, not amplitudes)
		N <- 2^input
		w <- exp(2*pi*i/N)
		n <- seq(0,N-1)
		QFTm <- 1/sqrt(N) * w^(outer(n,n))
		QFTm
	} else{							#input is ket to apply QFT to
		N <- length(input)
		w <- exp(2*pi*i/N)
		n <- seq(0,N-1)
		QFTm <- 1/sqrt(N) * w^(outer(n,n))
		QFTm %*% input
	}	
}
