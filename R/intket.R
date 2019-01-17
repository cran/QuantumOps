#generate a normalized ket (column vector) with arbitrary no of states
#' @export
intket <- function(x,n){
			if(x >= 2^n){
				print(paste("Warning:",n,"qubits are insufficient to represent encoded decimal",x))
			}
			b <- intToBits(x)											#get bits of int
			b <- rev(b)													#reverse
			b <- as.character(b)										#make character array
			b <- sapply(strsplit(b, "", fixed = TRUE), `[`, 2)			#remove leading 0's
			bl <- length(b)												#get length of array
			b <- b[(bl-n+1):bl]											#just want last few
			b <- as.integer(b)
			if(b[1] == 0){
				k <- ket(1,0)
			} else{
				k <- ket(0,1)
			}
			if(n > 1){
				for(j in 2:length(b)){
					if(b[j] == 0){
						k <- tensor(k,ket(1,0))
					} else{
						k <- tensor(k,ket(0,1))
					}
				}
			}
			k
}
