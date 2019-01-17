#install.packages("QuantumOps",repos=NULL,type="source")
#library("QuantumOps")
#' @export
Shor <- function(N,trials=150,random=FALSE){
	gcd <- function(x,y) {
	  r <- x%%y;
	  return(ifelse(r, gcd(y, r), y))
	}
	l <- ceiling(log(N,base=2))+1


	#p <- 3
	#q <- 5
	#N <- p*q		#Number to factor
	
	a <- 6

	M <- ceiling(sqrt(N))	#Known upper bound on a

	

	#Begin probabilistic quantum operations
	modNSuccess <- FALSE
	totaltries <- 0
	a <- 2
	while(!modNSuccess & totaltries < trials){
		totaltries <- totaltries + 1

		#a <- N
		#while( gcd(a,N) != 1)
		#	a <- sample(seq(2,N-1,by=1),1)			
		#"random" number
		if(random | ( N != 15 & N != 21 ) ){
			while(gcd(a,N) != 1)
				a <- a + 1
		} else{
			if(N == 15){
				a <- 2
			} else if(N == 21){
				a <- 8
			}
		}
		print(paste("Attempting to factor",N,"with a chosen random number",a))

		#Define function f(x) = a^x mod N
		#f <- function(x){a^x %% N}
		f <- exponentialMod(a,N)
		m <- Uf(f,l,l)
		#Test the function
		#print("Oracle output on classical data, should be periodic")
		#x <- seq(0,2^l-1,by=1)
		#print(f(x))

		PeriodTries <- 15
		success <- FALSE
		Ptries <- 0
		while( !success & Ptries < PeriodTries ){
			Ptries <- Ptries + 1

			#Set up input register
			v <- intket(0,l)

			#Apply H to input
			Hg <- H()
			for(j in 2:l)				#Build up Hadamard operator depending on input size
				Hg <- tensor(Hg,H())
			v <- U(Hg,v)

			#Add |0000> target register to v
			v <- tensor(v,intket(0,l))
			#print(paste("Input ket has length",length(v)))
			#print(dirac(v))

			#Do quantum oracle
			v <- U(m,v)
			#barplot(abs(t(v)))

			#Measure output of oracle
			#Measurement can be useful from a conceptual perspective
			#But not necessary in practice, see "A Course in Quantum Computing" by Loceff
			#v <- measure(v,5,6,7,8)		
			#print(paste("Measured ket has length",length(v)))
			#print(dirac(v))

			#QFT on input register
			v <- QFT(v)
			#barplot(abs(t(v)))

			#Find Max Values (just for info)
			#mvals <- rep(0,10)
			#absv <- abs(v)
			#for(j in 1:10){
			#	mvals[j] <- which.max(absv)
			#	absv[mvals[j]] <- 0
			#}
			#print("Max amplitudes")
			#print(mvals-1)

			vv <- measure(v)
			v <- vv[[1]]
			y <- vv[[2]]

			#y <- bitwShiftR(y,4)
			y <- floor(y/2^l)
			print(paste("Performed measurement on and got"))
			print(dirac(v))
			print(paste("Which means input register has value",y))

			frac <- CFA(y/2^4)
			p <- frac[2]

			print(paste("Continued fractions determined the period is",p))

			if(f(1) == f(1+p)){
				print(paste(f(1),f(1+p)))
				print("Period Finding: Success")
				if(p %% 2 == 1){
					print("Odd period: Failure (Reattempt)")
				} else{
					success <- TRUE
				}
			} else{
				print("Period Finding: Failure (Reattempt)")
			}
		}

		if(success){
			x <- a^(p/2)
			print(paste("Computed x is",x))

			print(paste("x mod N =",x %% N))
			if(x %% N == 1){
				print("Equal 1 (mod N) and therefore incorrect (Reattempt)")
			} else if(x %% N == N-1){
				print("Equal -1 (mod N) and therefore incorrect (Reattempt)")
			} else {
				modNSuccess <- TRUE
				print(paste("Factors of",N,"are",gcd(x+1,N),"and",gcd(x-1,N)))
				return(c(gcd(x+1,N),gcd(x-1,N)))
			}
		}
		print("Number of attempts exceeded, number not factored")
	}
}






	

