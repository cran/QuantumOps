#Compute the norm of wavefunction
#' @export
norm <- function(v){
	sqrt(inner(v,v))[1]	#which is the square root of the inner product with itself
}
