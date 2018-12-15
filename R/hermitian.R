#check if matrix is hermitian
#' @export
hermitian <- function(m){
	epsilon <- 1e-15
	i <- complex(1,0,1)
	#Equal to adjoin (is its own inverse)
	#all(m == adjoint(m)) #issues with precision prevent from working
	all(all(	Re(m) > Re(adjoint(m)-epsilon)		) && 
		all(	Re(m) < Re(adjoint(m)+epsilon)		) && 
		all(	Im(m) > Im(adjoint(m)-epsilon*i)	) && 
		all(	Im(m) < Im(adjoint(m)+epsilon*i)	) 
		)
}
