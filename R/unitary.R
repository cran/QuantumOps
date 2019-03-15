#check if matrix is unitary
#' @export
unitary <- function(m,epsilon=1e-13){
	i <- complex(1,0,1)
	#Inverse is equal to adjoint
	#all(solve(m) == adjoint(m)) #issues with precision prevent from working
	all(all(Re(solve(m)) > Re((adjoint(m)-epsilon)) 		) && 
		all(Re(solve(m)) < Re(adjoint(m)+epsilon) 			) && 
		all(Im(solve(m)) > Im((adjoint(m)-epsilon*i)) 		) && 
		all(Im(solve(m)) < Im(adjoint(m)+epsilon*i)  		)
		)
} 
