

#Take lists of cycles and generate one matrix
SynthesizeCircuit <- function(l){
	#Input can be list (if there's only 1 sequence of cycles) or list of list (to interleave them)
	if( !is.list(l[[1]]) )	#If it's just 1 list, make a list of 1 list, so the code works either way
		l <- list(l)

	M <- diag( dim(l[[1]][[1]])[1])	#Idenity gate with as many rows as the gates
	#go through each list, interleaving all of them
	#if some are longer, skip shorter ones when they are done
	finished <- FALSE
	idx <- 1
	while(!finished){
		finished <- TRUE
		for(j in 1:length(l)){						#Check each list
			if( length(l[[j]]) >= idx){				#If still some left
				finished <- FALSE
				M <- l[[j]][[idx]] %*% M			#Add it to matrix
			}
		}
		idx <- idx + 1
	}
	M
}
