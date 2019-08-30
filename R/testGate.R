#' @export
testGate <- function(g,inputs=0:(dim(g)[1]-1) ){
	N <- dim(g)[1]
	if( (log(N,base=2) %% 1 ) != 0 )
		print("Warning: Input gate is does not have dimension of a power of 2")
	#Test all possible input kets (basis state encoded that is)
	for(j in inputs){
		v <- intket(j,log(N,base=2))
		print(paste("Input: ",dirac(v)))
		print(paste("Output:",dirac( g %*% v)))
		print("")
	}
}





