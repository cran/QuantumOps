
CoherentNoise <- function(p,theta,randomRotation=FALSE){
	n <- log(dim(p)[1],base=2)	#Number of qubits in input density matrix
	if(!randomRotation){		#Constant Z rotations
		Cnoise <- many(gate=Rz(theta),n=n)
	}else{
		Rgates <- list( Rx(theta),Ry(theta),Rz(theta))	#Rx,Ry,Rz all by same angle
		idx <- sample(1:3,size=n,replace=TRUE)			#Pick n gates at random (of the 3)
		Cnoise <- Rgates[[idx[1]]]
		if(n > 1){						#Build tensor product of them all
			for(j in 2:n)
				Cnoise <- tensor(Cnoise,Rgates[[idx[j]]])
		}
	}
	Cnoise %*% p %*% adjoint(Cnoise)
}
