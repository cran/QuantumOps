
ranket <- function(n){
	i <- complex(1,0,1)
	theta <- runif(n=1,min=0,max=pi)		#Pick random polar coordinates
	phi <- runif(n=1,min=0,max=2*pi)
	v <- ket( cos(theta/2) , exp(i*phi)*sin(theta/2))	#Create ket
	if(n > 1){
		for(j in 2:n){					#Repeat for as many qubits speicified
			theta <- runif(n=1,min=0,max=pi)
			phi <- runif(n=1,min=0,max=2*pi)
			v <- tensor( v , ket( cos(theta/2) , exp(i*phi)*sin(theta/2)) )
		}
	}
	v
}
