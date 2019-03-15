#bit-wise mod 2 add two integers
#' @export
checkCases <- function(clauses,colorCode=FALSE){
	c <- dim(clauses)[1]			#number of clauses
	n <- dim(clauses)[2]			#number of qubits

	w <- seq(0,2^n-1,by=1)			#all possible values of ket
	b <- matrix(rep(0,2^n*n),nrow=2^n)
	for(j in n:1){				#create matrix of binary values 
		b[,j] <- w %% 2			# of all possible values
		w <- floor(w/2)
	}

	clauseCounter <- rep(0,2^n)		#fill with 0's at first
	for(j in 1:c){					#for each clause
		indices <- which( clauses[j,] == 0 | clauses[j,] == 1 )		#only indices that have 0 or 1
		subclause <- clauses[j,indices]								#subclause with only relevant bits
		B <- as.matrix(b[,indices])									#copy of binary matrix with just relevant bits
		cc <- rowSums(B == subclause[col(B)]) == ncol(B)
		cc[which(cc == TRUE)] <- 1
		cc[which(cc == FALSE)] <- 0
		clauseCounter <- clauseCounter + cc
	}
	if(!colorCode){
		clauseCounter/c
	} else{
		color <- rep("Violet",length(clauseCounter))
		clauseCounter <- clauseCounter/c
		color[which(as.numeric(clauseCounter) >= 0.2)] <- "Purple"
		color[which(as.numeric(clauseCounter) >= 0.4)] <- "Blue"
		color[which(as.numeric(clauseCounter) >= 0.6)] <- "Green"
		color[which(as.numeric(clauseCounter) >= 0.8)] <- "Yellow"
		color[which(as.numeric(clauseCounter) >= 0.9)] <- "Orange"
		color[which(as.numeric(clauseCounter) == 1)] <- "Red"	
		color
	}
		
}



