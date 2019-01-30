#make controlled version of input gate
#' @export
cntrld <- function(gate,n,...){
	qbitlist <- as.integer(list(...))				#list of qubits
	CQubits <- qbitlist[-length(qbitlist)]			#control qubits are all but last
	nCQubits <- length(CQubits)						#number of control qubits
	tQubit <- qbitlist[length(qbitlist)]			#target qubit is last
	
	d <- dim(gate)[1]		#side length of input gate
	D <- 2^n	#side length of controlled version
	g <- diag(1,D,D)		#create diagonal matrix

	v <- seq(0,D-1,by=1)			#binary indices
	b <- matrix(rep(0,D*log(D,base=2)),nrow=D)
	for(j in log(D,base=2):1){				#create matrix of binary values 
		b[,j] <- v %% 2			# of all possible values
		v <- floor(v/2)
	}


	CQubits <- CQubits + 1	#R indexes from 1
	tQubit <- tQubit + 1

	#Build the operator, which is gate applied just to target Qubit
	if(tQubit == 1){				#if target is first qubit
		operator <- gate			#it is gate, tensored with I for all other qubits
		for(j in 2:n)
			operator <- tensor(operator,I())
	} else{
		operator <- I()				#otherwise, first qubit gets I
		if(tQubit != 2){			#and all others up to target qubit
			for(j in 2:(tQubit-1))
				operator <- tensor(operator,I())
		}
		operator <- tensor(operator,gate)	#target qubit gets gate
		if(tQubit != n)
			for(j in (tQubit+1):n)			#I for all remaining qubits
				operator <- tensor(operator,I())
	}


	#For each basis state
	for(j in 0:D-1){
			if( all(b[j+1,CQubits] == 1) ){			#if all control qubits are 1
				g[j+1,] <- operator %*% intket(j,n)	#that column is operator applied to it (gate applied to just target qubit)
			}										#otherwise, it is the 1 on the diagonal (already)
	}
	g		#return the gate
}




#for(r in 0:1){
#		for(c in 0:1){
#			pp(r,c)
#			rows <- which( b[,CQubits] == 1 & b[,tQubit] == r)
#			cols <- which( b[,CQubits] == 1 & b[,tQubit] == c)
#			pp("rows",rows)
#			pp("cols",cols)
#			#g[  rows , cols  ] <- gate[r+1,c+1]
#			for(j in 1:length(rows)){
#				for(k in 1:length(cols)){
#					g[rows[j],cols[k]] <- gate[r+1,c+1]
#				}
#			}
#		}
#	}
