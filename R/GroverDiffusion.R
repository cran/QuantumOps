#' @export
GroverDiffusion <- function(input){
	i <- complex(1,0,1)
	if(length(input) == 1){				#input is constant specifying number of Qubits
		n <- input						#(inconsistent w/ QFT)
		N <- 2^n						#get dimension from qubit number
		GDm <- matrix(rep(2/N,N^2),nrow=N)	#Build Grover Diffusion matrix
		#Ig <- I()							#All elements are 2/N
		#for(j in 2:n)						#with additional -1 on diagonal
		#	Ig <- tensor(Ig,I())			#This is proper QuantumOps way to build
		Ig <- diag(N)						#This is proper R way to build
		GDm <- GDm - Ig
		GDm								#return gate
	} else{								#input is ket to apply Grover Diffusion to
		N <- length(input)				#get dimension from ket
		n <- log(N,base=2)				#extract # of qubits from dimension
		GDm <- matrix(rep(2/N,N^2),nrow=N)
		#Ig <- I()
		#for(j in 2:n)					#This is proper QuantumOps way to build
		#	Ig <- tensor(Ig,I())
		Ig <- diag(N)
		GDm <- GDm - Ig
		GDm %*% input					#return ket after gate applied
	}	
}
