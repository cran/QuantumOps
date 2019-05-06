#bit-wise mod 2 add two integers
#' @export
reduceMeasure <- function(...,l2r=FALSE){
	input <- list(...)
	qstate <- unlist(input[[1]])
	p <- probs(as.complex(qstate))
	m <- sample( seq(1,length(qstate),by=1), size=1, prob=p )		#pick a state according to probability
	#print(paste("Measured state:",m-1))

	if( length(input) == 1){		#input is just a ket, measure all qubits	
		amplitudes <- rep(0,length(qstate))
		amplitudes[m] <- 1
		k <- do.call(ket, as.list(amplitudes) )
		list(k,m-1)
	} else{											#input it ket, followed by list of measured qubits
		n <- log(length(input[[1]]),base=2)			#length of input ket (in qubits so log2)

		mqubits <- array(as.numeric(unlist(input)[-(seq(1,2^n,by=1))]))	#get list of qubits to measure
																		#doesn't include leading ket

		if(!l2r)
			mqubits <- n - mqubits		#indexed in reverse (Quantum convention)
		else
			mqubits <- mqubits + 1		#R indexes from 1
		
		v <- seq(0,2^n-1,by=1)			#all possible values of ket
		b <- matrix(rep(0,2^n*n),nrow=2^n)
		for(j in n:1){				#create matrix of binary values 
			b[,j] <- v %% 2			# of all possible values
			v <- floor(v/2)
		}

		# keep only rows in which all (measured) bits are equal to measured qubits (mqubits)
		#Find rows that match row m on all mqubit columns
		if(	length(mqubits) > 1 ){
			#extra code --
			
			check.equal <- function(x,y){
				isTRUE(all.equal(y, x, check.attributes=FALSE))	#Thanks to Ista Zahn
			}
			indices <- which(apply(b[,mqubits],1,check.equal,b[m,mqubits]))	#All measure qubits match
		} else {	
			indices <- which( b[,mqubits] == b[m,mqubits] )					#Single measured qubit matches
		}

		#indices is list of indices at which state could still be
		amplitudes <- input[[1]][indices]				#take subset of input ket
		k <- do.call(ket,as.list(amplitudes))
		list(k,m-1,b[m,mqubits])
	}
}



