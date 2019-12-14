
#Generate a circuit for n-bit addition
#As from "Quantum full adder and subtractor" by Kai-Wen Cheng and Chien-Cheng Tseng
nBitAddition <- function(n){
	g <- list()
	for(j in 1:n){
		idx <- (j-1)*3
		g <- c(g, FullAdder(n=3*n+1,cin=idx,a=idx+1,b=idx+2,cout=idx+3))
	}
	g
}



#input ket must be
# C0 a1 b1 C1 a2 b2 C2 ... Cn
# And the C's must be 0
#thus the input ket is 3n+1 qubits
