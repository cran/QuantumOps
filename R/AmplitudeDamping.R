
#p is DM, Pad is probability of amplitude damping
AmplitudeDamping <- function(p,Pad){
	n <- log(dim(p)[1],base=2)	#Number of qubits in input density matrix
	K1 <- mm(1,0,0,sqrt(1-Pad))	#Nothing happens
	K2 <- mm(0,0,sqrt(Pad),0)	#Damping happens
	
	#Apply amplitude damping noise (channel) to each qubit
	for(j in 1:n){
		k1 <- single(gate=K1,n=n,t=j-1)
		k2 <- single(gate=K2,n=n,t=j-1)
		p <- k1 %*% p %*% adjoint(k1) + k2 %*% p %*% adjoint(k2)
	}
	p
}
