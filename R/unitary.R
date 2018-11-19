#check if matrix is unitary
#' @export
unitary <- function(m){
	#Inverse is equal to adjoint
	#all(solve(m) == adjoint(m)) #issues with precision prevent from working
	all( Re(solve(m)) > Re((adjoint(m)-1e-15)) && Re(solve(m)) < Re(adjoint(m)+1e-15) && Im(solve(m)) > Im((adjoint(m)-1e-15*i)) && Im(solve(m)) < Im(adjoint(m)+1e-15*i) )
}
