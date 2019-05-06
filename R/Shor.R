#install.packages("QuantumOps",repos=NULL,type="source")
#library("QuantumOps")
#' @export
Shor <- function(N,trials=150,random=FALSE){
	gcd <- function(x,y) {
	  r <- x%%y;
	  return(ifelse(r, gcd(y, r), y))
	}
	l <- ceiling(log(N,base=2))+1						#Number of qubits needed
	M <- ceiling(sqrt(N))								#Known upper bound on a (period)

	

	#Begin probabilistic quantum operations
	modNSuccess <- FALSE
	totaltries <- 0
	a <- 2
	while(!modNSuccess & totaltries < trials){
		totaltries <- totaltries + 1
		
		#"random" number
		if(random | ( N != 15 & N != 21 ) ){
			while(gcd(a,N) != 1)
				a <- a + 1
		} else{						#Know these numbers work well
			if(N == 15){			#In general, these are random because number to factor is new
				a <- 2
			} else if(N == 21){
				a <- 8
			}
		}
		print(paste("Attempting to factor",N,"with a chosen random number",a))

		#Define function f(x) = a^x mod N		Period of this function is related to prime factors
		f <- exponentialMod(a,N)				#Replaces f <- function(x){a^x %% N}
		m <- Uf(f,l,l)							#m is f in unitary matrix form (quantum oracle)

		#Try to find period of function with quantum operations
		PeriodTries <- 15
		success <- FALSE
		Ptries <- 0
		while( !success & Ptries < PeriodTries ){
			Ptries <- Ptries + 1

			#Set up input register
			v <- intket(0,l)			#l qubits all at |0>

			#Apply H to input
			v <- many(H(),l,v)			#Apply H gate to all qubits

			#Add |0000> target register to v
			v <- tensor(v,intket(0,l))

			#Do quantum oracle
			v <- U(m,v)

			#QFT on input register
			v <- QFT(v)
			#barplot(abs(t(v)))

			#Measure the ket
			vv <- measure(v)
			v <- vv[[1]]			#The ket after measurement
			y <- vv[[2]]			#Integer value of state that was measured

			y <- floor(y/2^l)		#take just the first l qubits
			print(paste("Quantum measurement produces",dirac(v),"  Input Register =",y))

			frac <- CFA(y/2^4)		#Use continued fractions algorithm to classically find period from measured value
			p <- frac[2]			#2nd element returned from function

			print(paste("CFA finds period",p))

			if(f(1) == f(1+p)){									#Test periodicity
				print("Correct period found")
				if(p %% 2 == 1){
					print("Failure (Odd Period)")
				} else{
					success <- TRUE
				}
			} else{
				print("Period Finding: Failure (Reattempt)")
			}
		}

		if(success){
			x <- a^(p/2)
			print(paste("Value derives from a and p, x =",x))

			print(paste("x mod N =",x %% N))
			if(x %% N == 1){
				print("Equal 1 (mod N) and therefore incorrect (Reattempt)")
			} else if(x %% N == N-1){
				print("Equal -1 (mod N) and therefore incorrect (Reattempt)")
			} else {
				modNSuccess <- TRUE
				print(paste("Success: Factors of",N,"are",gcd(x+1,N),"and",gcd(x-1,N)))
				return(c(gcd(x+1,N),gcd(x-1,N)))
			}
		}
	}
	print("Number of attempts exceeded, number not factored")
}






	

