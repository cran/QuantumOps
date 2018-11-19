#check if matrix is hermitian
#' @export
hermitian <- function(m){
	#Equal to adjoin (is its own inverse)
	#all(m == adjoint(m)) #issues with precision prevent from working
	all( Re(m) > Re((adjoint(m)-1e-15)) && Re(m) < Re(adjoint(m)+1e-15) && Im(m) > Im((adjoint(m)-1e-15*i)) && Im(m) < Im(adjoint(m)+1e-15*i) )
}
