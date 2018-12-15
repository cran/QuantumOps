#make controlled version of input gate
#' @export
controlled <- function(gate,nCQubits=1){
	d <- dim(gate)[1]		#side length of input gate
	D <- 2^(nCQubits+1)		#side legnth of controlled version
	g <- diag(1,D,D)		#create diagonal matrix
	g[ (D-d+1):D , (D-d+1):D ] <- gate	#set lower right (all control qubits = 1) to input gate
	g
}
