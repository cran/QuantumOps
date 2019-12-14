#' @export
single <- function(gate,n,t,...){	
	#Create matrix for a single qubit gate in a large system
	t <- t+1			#indexed from 0, but kets are indexed from 1 in R
	#For indexing in R makes this over complicated
	if(t == 1){			#t is target qubit
		g <- gate
		if(n > 1){
			for(j in 2:n)
				g <- tensor(g,I())
		}
	} else if(t == n){
		g <- I()
		if(t > 2){
			for(j in 2:(t-1))
				g <- tensor(g,I())
		}
		g <- tensor(g,gate)
	} else{
		g <- I()
		if(t > 2){
			for(j in 2:(t-1))
				g <- tensor(g,I())
		}
		g <- tensor(g,gate)
		for(j in (t+1):n)
			g <- tensor(g,I())
	}
	
	ket <- list(...)
	if(length(ket) == 0){
		g
	} else{
		g %*% ket[[1]]
	}	
}
