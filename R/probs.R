#probability vector of measuring each value
#' @export
probs <- function(ket){
	#print(paste("Meas 0 Prob:",abs(x[1,1])^2,"Meas 1 Prob:",abs(x[2,1])^2))
	abs(ket)^2
}
