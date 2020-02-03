#' @export
QFT <- function(input,byCycle=FALSE,swaps=TRUE,CliffordT=FALSE,prec=10,path="./"){
	i <- complex(1,0,1)
	if(length(input) == 1){				#input is constant specifying dimension (in # of qubits, not amplitudes)
		if(byCycle == FALSE){			#Return as 1 matrix
			N <- 2^input
			w <- exp(2*pi*i/N)
			n <- seq(0,N-1)
			QFTm <- 1/sqrt(N) * w^(outer(n,n))
			QFTm
		} else{							#Return as list of gates for each cycle


			n <- input
			QFTm <- list()				#Start with empty list
			if(!CliffordT){				#Have to fix issue where Clifford+T requires an ancilla
				nQubits <- n
			}else{
				nQubits <- n+1
			}

			for(j in 0:(n-1)){
				QFTm <- c(QFTm,list( single(gate=H(),n=nQubits,t=j) ))	#Hadamard qubit j
				if(j < n-1){
					if(!CliffordT){	#Assume rotation operations are possible
						for(k in (j+1):(n-1) ){
							Rg <- R( (2*pi) / (2^(k-j+1)) )
							g <- controlled( gate= Rg , n=n , tQubit=j, cQubits=k )
							QFTm <- c(QFTm,list( g ))
						}
					}else{	#Give the Clifford+T version
						print(paste("Decomposing cycle",j+1))	
						for(k in (j+1):(n-1) ){
							print(paste("gate",k))
							angle <- (2*pi) / (2^(k-j+1))
							CKT <- DecomposeGate(path=path,g=angle,n=n,TwoQubit=TRUE,
										tQubit=j,cQubit=k,prec=prec)
							QFTm <- c(QFTm,CKT)
						}
					}
				}
			}
			if(swaps){					#If to include trailing swap operations
				#SWAP operations
				for(j in 0:(floor(n/2)-1)){
					#SWAPs implemented with 3 CNOT gates
					QFTm <- c(QFTm,list( controlled(gate=X(),n=nQubits,cQubits=j,tQubit=n-1-j) ))
					QFTm <- c(QFTm,list( controlled(gate=X(),n=nQubits,cQubits=n-1-j,tQubit=j) ))
					QFTm <- c(QFTm,list( controlled(gate=X(),n=nQubits,cQubits=j,tQubit=n-1-j) ))
				}
			}
			QFTm

		}
	} else{							#input is ket to apply QFT to
		N <- length(input)
		w <- exp(2*pi*i/N)
		n <- seq(0,N-1)
		QFTm <- 1/sqrt(N) * w^(outer(n,n))
		QFTm %*% input
	}	
}
