#' @export
CFA <- function(y,epsilon=1e-2){
	x <- y

	a0 <- floor(x)
	frac <- x - floor(x)
	x <- 1/frac
	n0 <- a0
	d0 <- 1
	if(frac == 0)
		return(c(n0,d0))

	a1 <- floor(x)
	frac <- x - floor(x)
	x <- 1/frac
	n1 <- a1*a0+1
	d1 <- a1
	if(frac == 0)
		return(c(n1,d1))

	nk <- 1				#Current numerator
	dk <- 1				#Current denominator
	n <- c(n1,n0)		#past numerators
	d <- c(d1,d0)		#past denominators
	
	
	while( abs(y-nk/dk) > epsilon ){
		a <- floor(x)
		frac <- x - floor(x)
		x <- 1/frac
		
		nk <- a*n[1] + n[2]
		dk <- a*d[1] + d[2]
		
		if(frac == 0)
			break

		n[2] <- n[1]
		d[2] <- d[1]
		n[1] <- nk
		d[1] <- dk
	}
	c(nk,dk)
}



