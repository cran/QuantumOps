
#p is DM, Ppd is probability of phase damping
PhaseDamping <- function(p,Ppd){
	#Change format a bit
	pz <- (1-sqrt(1-Ppd))/2

	n <- log(dim(p)[1],base=2)	#Number of qubits in input density matrix
	K1 <- mm(sqrt(1-pz),0,0,sqrt(1-pz))	#Nothing happens
	K2 <- mm(sqrt(pz),0,0,-sqrt(pz))	#Damping happens
	
	#Apply phase damping noise (channel) to each qubit
	for(j in 1:n){
		k1 <- single(gate=K1,n=n,t=j-1)
		k2 <- single(gate=K2,n=n,t=j-1)
		p <- k1 %*% p %*% adjoint(k1) + k2 %*% p %*% adjoint(k2)
	}
	p
}
