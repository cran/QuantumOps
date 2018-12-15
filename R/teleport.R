#shows process of teleporting a single qubit
#' @export
teleport <- function(v){
	#Create EPR pair (BELL state 00)
	A <- ket(1,0)			#Both initially in |0> state
	B <- ket(1,0)
	A <- H(A)				#H gate on qubit A
	AB <- tensor(A,B)		#Combine into 1 ket (seperable state)
	AB <- CX(AB)			#CNOT
	
	#Create combined system, input state with EPR pair
	vAB <- tensor(v,AB)
	print("Initial state")
	dirac(vAB)
	
	#CNOT, v is control qubit, A is target, I (no-op) on B
	vAB <- U(CX(),I(),vAB)
	#H on v, I (no-op) on A and B
	vAB <- U(H(),I(),I(),vAB)
	print("Pre-Measurement")
	dirac(vAB)

	print("If measure 00, state of teleported ket is")
	dirac(ket(vAB[1,1],vAB[2,1]))
	print("If measure 01, state of teleported ket is")
	dirac(ket(vAB[3,1],vAB[4,1]))
	print("If measure 10, state of teleported ket is")
	dirac(ket(vAB[5,1],vAB[6,1]))
	print("If measure 11, state of teleported ket is")
	dirac(ket(vAB[7,1],vAB[8,1]))
}
