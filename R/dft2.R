#bit-wise mod 2 add two integers
#' @export
dft2 <- function(f){
	i <- complex(1,0,1)
	N <- length(f)
	w <- exp(2*pi*i/N)						#Nth root of unity
	wm <- matrix(NA,nrow=N,ncol=N)			#Create matrix
	for(j in 0:N-1)
		for(k in 0:N-1)
			wm[j+1,k+1] <- w^(-j*k)
	F <- 1/sqrt(N)*rowSums(wm %*% f)
}
