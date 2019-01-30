#' @export
G <- function(a,b,g,p=0,...){
	i <- complex(1,0,1)	
	G <- exp(i*p)* matrix(c(	exp(i*b)*cos(a),
								-exp(-i*g)*sin(a),
								exp(i*g)*sin(a),
								exp(-i*b)*cos(a)
	),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		G
	} else {
		k <- G %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])	
	}
}
