#make controlled version of input gate
#' @export
GroversAlgorithm <- function(n,w,iterations=n,printOutput=FALSE,plotOutput=FALSE,tag=""){
	v <- intket(0,n+1)					#Prepare |00..0> state with n qubits
	Hg <- H()
	for(j in 2:(n+1))
		Hg <- tensor(Hg,H())			#Build n+1 qubit Hadamard gate
	v <- U(Hg,v)						#Apply Hadamard gate
	GOm <- GroverOracle(w,n+1)			#Grovers Oracle matrix, search for w with n+1 qubits
	GDm <- GroverDiffusion(n)			#Grovers Diffusion matrix, with n qubits
	GDm <- tensor(GDm,I())				#Last qubit is not affected by Diffusion, so add Identity op

	#Loop n = logN times (by default)
	P <- rep(NA,iterations)
	for(j in 1:iterations){
		v <- U(GOm,v)			#Apply Oracle
		v <- U(GDm,v)			#Apply Diffusion
		
		P[j] <- probs(v)[w+1,1]	#Probability of measuring state w
		if(printOutput)
			pp("Propability of measuring",w,"at iteration",j,":",P[j])
	}
	if(plotOutput){
		jpeg(paste("GroversAlgorithm",tag,".jpg",sep=""))
		barplot(P,main=paste("Probability of measuring state",w),ylab="Probability",xlab="Iterations",col="Blue")
		dev.off()
	}
	v			#Return ket
}
