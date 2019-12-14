
PauliNoise <- function(p,e=ex+ey+ez,ex=e/3,ey=e/3,ez=e/3){
	n <- log(dim(p)[1],base=2)	#Number of qubits in input density matrix
	allX <- many(gate=X(),n=n)
	allY <- many(gate=Y(),n=n)
	allZ <- many(gate=Z(),n=n)
	(1-e) * p  +  ex * allX %*% p %*% adjoint(allX)  +  ey * allY %*% p %*% adjoint(allY)  +  ez * allZ %*% p %*% adjoint(allZ)
}
