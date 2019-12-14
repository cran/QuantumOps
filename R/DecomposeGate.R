

controlT <- function(tQubit,cQubit,ancilla,n,scaler=1){
	t <- tQubit; c <- cQubit; a <- ancilla;	#For convenience
	CKT <- list()
	#CNOT(c,t) H(a)
	CKT <- c(CKT,list(		controlled(n=n,gate=X()*scaler,cQubits=c,tQubit=t) %*% single(gate=H(),n=n,t=a) ))
	#P'(c) CNOT(t,a)
	CKT <- c(CKT,list(		single(gate=adjoint(S()),n=n,t=cQubit) %*% controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#CNOT(a,c)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=a,tQubit=c) ))
	#T(c), T'(a)
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=c) %*% single(gate=adjoint(T()),n=n,t=a) ))
	#CNOT(t,c)	
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=c) ))
	#CNOT(t,a)	
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#T(c) T'(a)
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=c) %*% single(gate=adjoint(T()),n=n,t=a) ))
	#CNOT(c,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=a) ))
	#H(c)	
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=c) ))
	#T(c)	
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=c) ))
	#H(c)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=c) ))
	#CNOT(c,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=a) ))
	#T'(c) T(a)
	CKT <- c(CKT,list(		single(gate=adjoint(T()),n=n,t=c) %*% single(gate=T(),n=n,t=a) ))
	#CNOT(t,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#CNOT(t,c) T(a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=c) %*% single(gate=T(),n=n,t=a) ))
	#T'(c)	
	CKT <- c(CKT,list(		single(gate=adjoint(T()),n=n,t=c) ))
	#CNOT(a,c)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=a,tQubit=c) ))
	#S(c) CNOT(t,a)
	CKT <- c(CKT,list(		single(gate=S(),n=n,t=c) %*% controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#CNOT(c,t) H(a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=t) %*% single(gate=H(),n=n,t=a) ))
	CKT
}

if(FALSE){
controlH <- function(tQubit,cQubit,n){
	t <- tQubit; c <- cQubit;	#For convenience
	CKT <- list()
	#H(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=t) ))
	#S(t)
	CKT <- c(CKT,list(		single(gate=S(),n=n,t=t) ))
	#H(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=t) ))
	#CNOT(t,c)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=c) ))
	#T(c) T(t)
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=c) %*% single(gate=T(),n=n,t=t) ))
	#H(c) H(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=c) %*% single(gate=H(),n=n,t=t) ))
	#CNOT(c,t)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=t) ))
	#CNOT(t,c)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=c) ))
	#H(c) S(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=c) %*% single(gate=S(),n=n,t=t) ))
	CKT
}
}

controlH <- function(tQubit,cQubit,n,scaler=1){
	t <- tQubit; c <- cQubit;	#For convenience
	CKT <- list()
	#P(t)
	CKT <- c(CKT,list(		single(gate=S()*scaler,n=n,t=t) ))
	#H(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=t) ))
	#T(t)
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=t) ))
	#CNOT(c,t)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=t) ))
	#T'(t)
	CKT <- c(CKT,list(		single(gate=adjoint(T()),n=n,t=t) ))
	#H(t)
	CKT <- c(CKT,list(		single(gate=H(),n=n,t=t) ))
	#S'(t)
	CKT <- c(CKT,list(		single(gate=adjoint(S()),n=n,t=t) ))
	CKT
}


controlS <- function(tQubit,cQubit,ancilla,n,scaler=1){
	t <- tQubit; c <- cQubit; a <- ancilla;	#For convenience
	CKT <- list()
	CKT <- c(CKT,list(		single(gate=I()*scaler,n=n,t=t)))
	#CNOT(c,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=a) ))
	#CNOT(t,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#T(c) T(t) T'(a)
	CKT <- c(CKT,list(		single(gate=T(),n=n,t=c) %*% single(gate=T(),n=n,t=t) %*% single(gate=adjoint(T()),n=n,t=a) ))
	#CNOT(t,a)	
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=t,tQubit=a) ))
	#CNOT(c,a)
	CKT <- c(CKT,list(		controlled(gate=X(),n=n,cQubits=c,tQubit=a) ))
	CKT
}
i <- complex(1,0,1)
w <- exp(i*pi/4)

#g is the (alpha,beta,gamma) of G(alpha,beta,gamma,0) that should be approximated, IF THIS IS A SINGLE Z rotation, g is the angle!
#2Qubit specifies if this is a controlled 2-qubit gate
#n is the total number of qubits in the system
#tQubit and cQubit are used to indicate which is the target and control
#assume n is the number of qubits in the original, the output will be n+1 for the required ancilla
DecomposeGate <- function(path,g,TwoQubit=FALSE,n=1,tQubit=0,cQubit=1,prec=10){
	#If this is an arbitrary gate
	#Convert g to Rz(beta)Rgamma)Rz(delta) = Rz(beta)HRz(gamma)HRz(delta)
	if(length(g) == 3){	
		alpha <- g[1]; beta <- g[2]; gamma <- g[3];
		Rz1 <- -beta + gamma + pi/2
		Rx1 <- 2*alpha
		Rz2 <- -beta - gamma - pi/2
		angles <- c(Rz1,Rx1,Rz2)
	}else if(length(g) == 1){
		angles <- g	#g
	}else{
		print("Error [Decompose Gate]: g must have length of 1 or 3")
	}
	angles <- paste("'(",angles,")'",sep="")
	#Now have beta,gamma, and delta
	CKT <- list()		#Going to create a list of cycles to implement this gate
	#For each of the 3 stages, we are going to decompose the rotations of sequences of H,T,and S gates
	#If a single qubit, it is just these gates
	#If a 2-qubit gate, each H,T, and S gate must be made into a sequence for the control, target, and ancilla
	#Call the functions for each with tQubit,cQubit, and n	
	#For each angle, 1 or 3
	for(angle in 1:length(angles)){
		#call gridsynth to produce the H,T,S approximation
		number <- sample(1:10000000,size=1)
		sFile <- paste(path,"/sequence",number,sep="")
		cmd <- paste(path,"/gridsynth ",angles[angle]," -b ",prec," > ",sFile,sep="")	#store in sequence
		system(cmd)								#external call
		gates <- readLines(sFile)			#read sequence
		file.remove(sFile)
		l <- nchar(gates)							#get the length
		scaler <- 1
		if(!TwoQubit){
			PhaseNeeded <- FALSE
			for(j in l:1){
				x <- substr(gates,j,j)		#Depending on the character
				if(x != 'W' && PhaseNeeded){
					#CKT <- c(CKT, list( single(gate=I()*scaler,n=n+1,t=tQubit) ))
					PhaseNeeded <- FALSE
				}
				if(x == 'H')			#Insert a signle H,T,or S gate
					CKT <- c(CKT, list( single(gate=H(),n=n,t=tQubit) ))
				if(x == 'T')
					CKT <- c(CKT, list( single(gate=T(),n=n,t=tQubit) ))
				if(x == 'S')
					CKT <- c(CKT, list( single(gate=S(),n=n,t=tQubit) ))
				if(x == 'X')
					CKT <- c(CKT, list( single(gate=X(),n=n,t=tQubit) ))
				if(x == 'W'){
					scaler <- scaler * w
					PhaseNeeded <- TRUE
				}
			}
		#	CKT[[1]] <- CKT[[1]] * scaler	
		}
		if(TwoQubit){
			PhaseNeeded <- FALSE
			for(j in l:1){
				x <- substr(gates,j,j)		#Depending on the character
				if(x != 'W' && PhaseNeeded){
					#CKT <- c(CKT,list( single(gate=I()*scaler,n=n+1,t=tQubit) ))
					#CKT <- c(CKT,list( single(gate=I()*scaler,n=n+1,t=cQubit) ))
					#CKT <- c(CKT,list( controlled(gate=I()*-scaler,n=n+1,cQubits=cQubit,tQubit=tQubit) ))
				#	scaler <- 1
					PhaseNeeded <- FALSE
				}
				if(x == 'H')			#Insert a sequence for controlled H,T,or S gate
					CKT <- c(CKT, controlH(cQubit=cQubit,tQubit=tQubit,n=n+1) ) 
				if(x == 'T')
					CKT <- c(CKT, controlT(cQubit=cQubit,tQubit=tQubit,ancilla=n,n=n+1))
				if(x == 'S')
					CKT <- c(CKT, controlS(cQubit=cQubit,tQubit=tQubit,ancilla=n,n=n+1)) 
				if(x == 'X')
					CKT <- c(CKT,list( controlled(gate=X(),cQubits=cQubit,tQubit=tQubit,n=n+1) ))
				if(x == 'W'){
					scaler <- scaler * w
					PhaseNeeded <- TRUE
				}
				#if(x != 'W'){
				#	scaler <- 1
				#}
			}
		}
		if( length(angles) == 3 && angle < 3){	#If there are 3 angles, and we're on 1 or 2, insert the H gate after
			CKT <- c(CKT, list( single(gate=H(),n=n+1,t=tQubit) ))	#Mental not, not need 2b controlled?
		}
	}
	CKT
}


synthTest <- function(g){
	i <- complex(1,0,1)
	w <- exp(i*pi/4)
	cmd <- paste("./gridsynth/gridsynth pi/",g," > gridsynth/sequence",sep="")
system(cmd)
	gates <- readLines("gridsynth/sequence")
	l <- nchar(gates)
	ag <- I()
	for(j in l:1){
		x <- substr(gates,j,j)
		if(x == 'H'){
			#cat('H')
			ag <- H() %*% ag
		}
		else if(x == 'T'){
			ag <- T() %*% ag
			#cat('T')
		}
		else if(x == 'S'){
			ag <- S() %*% ag
		#	cat('S')
		}
		else if(x == 'X'){
			ag <- X() %*% ag
		#	cat('X')
		}else if(x == 'W'){
			#ag <- w*ag
		#	cat('W')
		}else{
			print(paste("Not used:",x))
		}
	
	}	
	ag
}
