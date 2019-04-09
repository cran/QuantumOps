#' @export
QuantumMNIST256Classifier <- function(
	data=NULL,labels=NULL,digit=0,
	eta=1,decay=1,bsc=1,t=20,tag="",pl=TRUE,train=TRUE,
	validT=FALSE,vdata=NULL,vlabels=NULL,
	pretrained=FALSE,alpha=NULL,beta=NULL,gamma=NULL){

	filename <- "QNNout"
	imagename <- "QNNprobs"
	if(tag != ""){
		filename <- paste(filename,"_",tag,sep="")
		imagename <- paste(imagename,"_",tag,sep="")
	}

	#List of 33 gates
	if(!pretrained){
		alpha <- runif(33,0,2*pi)		#vector of all alpha parameters
		beta <- runif(33,0,2*pi)		#beta parameters
		gamma <- runif(33,0,2*pi)		#gamma parameters
	}
	g <- vector("list",33)			#list of gates

	#for(j in 1:33)
	#	g[[j]] <- G(alpha[j],beta[j],gamma[j])

	dC_da <- rep(0,33)	#gradients
	dC_db <- rep(0,33)
	dC_dg <- rep(0,33)

	bias <- 0.5				#bias added to output
	dC_dbias <- 0			#gradient
	
	#ckt building function
	#param will set a gate to the derivative wrt that paramter
	#which indicates whether its the first or 2nd term (relevant for beta and gamma)
	#gateNo is the gate identifier
	#invert flips the sign (relevant for controlled gates)
	ckt <- function(param="",which,gateNo,invert=FALSE){

		#copy set of gates to temproray gate set
		gc <- g
		if(param == "a"){
			gc[[gateNo]] <- G(alpha[gateNo]+pi/2,beta[gateNo],gamma[gateNo])		#replace 1 gate with derivatie wrt alpha
		} else if(param == "b"){
			if(which == 1){
				gc[[gateNo]] <- G(alpha[gateNo],beta[gateNo]+pi/2,0)				#replace 1 gate with derivatie wrt beta
			} else{
				gc[[gateNo]] <- G(alpha[gateNo],beta[gateNo]+pi/2,pi)
			}
		} else if(param == "g"){
			if(which == 1){
				gc[[gateNo]] <- G(alpha[gateNo],0,gamma[gateNo]+pi/2)				#replace 1 gate with derivatie wrt gamma
			} else{
				gc[[gateNo]] <- G(alpha[gateNo],pi,gamma[gateNo]+pi/2)
			}
		}
		if(param != "")
			gc[[gateNo]] <- -1*gc[[gateNo]]

		#Construct  circuit
		m <- vector("list",19)		#19 cycles
		m[[1]] <- gc[[1]]			#Cycle 1 - 8 G gates in parallel
		for(j in 1:7)
			m[[1]] <- tensor(m[[1]],gc[[1+j]])

		m[[2]] <- cntrld(gc[[9]],8,0,7)		#G9, Cntrl=Q0, T=Q7, on 8 qubits
	
		for(j in 1:7)				#Cycles 3-9
			m[[j+2]] <- cntrld(gc[[j+9]],8,8-j,7-j)	#

		m[[10]] <- gc[[17]]			#Cycle 10 - 8 G gates in parallel
		for(j in 1:7)
			m[[10]] <- tensor(m[[10]],gc[[17+j]])

		m[[11]] <- cntrld(gc[[25]],8,0,5)
		m[[12]] <- cntrld(gc[[26]],8,5,2)
		m[[13]] <- cntrld(gc[[27]],8,2,7)
		m[[14]] <- cntrld(gc[[28]],8,7,4)
		m[[15]] <- cntrld(gc[[29]],8,4,1)
		m[[16]] <- cntrld(gc[[30]],8,1,6)
		m[[17]] <- cntrld(gc[[31]],8,6,3)
		m[[18]] <- cntrld(gc[[32]],8,3,0)

		m[[19]] <- gc[[33]]
		for(j in 1:7)
			m[[19]] <- tensor(m[[19]],I())
		m
	}

	#apply ckt function
	qapp <- function(v,m){
		vv <- v
		for(j in 1:19){
			vv <- m[[j]] %*% vv
		}
		vv
	}

	#probability of measuring 1
	prob1 <- function(v){
		sum(probs(v)[128:256])
	}

	Zop <- tensor(Z(),I(),I(),I(),I(),I(),I(),I())
	#Re{ <Ut x |z|U x>}
	cmpr <- function(v,w){
		#prob1(v) - prob1(w)
		Re(adjoint(v) %*% Zop %*% w)
	}

	#Run Network on validation input set
	networkValidate <- function(m){
		N <- dim(vdata)[1]							#number of samples in validation set		
		p <- rep(0,N)		
		for(j in 1:N){								#for each validation input
			v <- do.call(ket,as.list(vdata[j,]))	#create ket of input
			v <- qapp(v,m)							#apply the circuit
			p[j] <- prob1(v)						#probability of measuring 1
			if( vlabels[j] != digit)
				p[j] <- 1 - p[j]					#flip if target digit is different than current input
		}
		write(pp("Network achieves ",mean(p),"accuracy on validation set"),file=filename,append=TRUE)
	}

	#Run Network on input set
	networkTest <- function(m){
		N <- dim(data)[1]							#number of samples in validation set		
		p <- rep(0,N)		
		for(j in 1:N){								#for each validation input
			v <- do.call(ket,as.list(data[j,]))	#create ket of input
			v <- qapp(v,m)							#apply the circuit
			p[j] <- prob1(v)						#probability of measuring 1
			if( labels[j] != digit)
				p[j] <- 1 - p[j]					#flip if target digit is different than current input
		}
		write(paste("Network achieves",mean(p),"accuracy on data set"),file=filename)
	}


	write("QNN start",file=filename)
	
	#input ket
	#amplitudes <- runif(256,0,1)
	#x <- do.call(ket,as.list(amplitudes))
	#target
	#y <- 0	

	#If training the network
	if(train){
		N <- length(labels)		#N is the number of samples provided in training set
		Nt <- sum(labels == digit)	#Nt is the number of samples that are the target digit
	
		#Rows are different digits, target and sum of all rest
		pix <- matrix(rep(0,2*t),nrow=2)		#pi(x;theta,b) = output of network
		p1 <- matrix(rep(0,2*t),nrow=2)		#probability of measuring 1 on output qubit

		#Generate modified circuits and create modified outputs to find gradients
		isControlled <- c(rep(FALSE,8), rep(TRUE,8), rep(FALSE,8), rep(TRUE,8), FALSE)		#Keep track of which gates are controlled (extra computation)
		#Number of training iterations
		for(tr in 1:t){
			print(paste("Iteration",tr,"of",t))

			#For each sample provided
			for(s in 1:N){
				cat(paste(s," "))

				#Set up input
				x <- do.call(ket,as.list(data[s,]))	#encode 256 integers as the amplitudes of a ket
				#And target
				y <- 0							#0 for all digits
				if(labels[s] == digit)			#except target
					y <- 1

				#Build gates
				for(j in 1:33){
					g[[j]] <- G(alpha[j],beta[j],gamma[j])
					if(!unitary(g[[j]])){
						write(paste("Error: Gate",j,"not unitary. Returning alpha,beta,gamma,bias"),file=filename,append=TRUE)
						return(list(alpha,beta,gamma,bias))
					}
				}

				#get "normal" ckt
				m <- ckt()

				#Do a validation run
				if(validT)
					networkValidate(m)

				#Apply input
				p <- qapp(x,m)

				Ep <- prob1(p)				#Get probability of measuring 1
				px <- Ep + bias				#Get output of network

				p1[y+1,tr] <- p1[y+1,tr] + Ep		#Record for overall training graph
				pix[y+1,tr] <- pix[y+1,tr] + px		#normalize later
				write(paste("px:",px,"   Prob 1:",Ep," bias:",bias," input:",labels[s]," target:",y),file=filename,append=TRUE)	#Log
				#Check for error
				if(Ep > 1 | Ep < 0){
					write("An error has occured, returning gates, matrix, input ket, and p ket for testing purposes",file=filename,append=TRUE)
					return(list(g,m,x,p))
				}
				#if( abs(px - y) < prec){
				#	write("Probability with precision, stopping early",file=filename,append=TRUE)
				#	t <- tr		#set trials to lower number
				#	break
				#}

				dC_dbias <- (px-y)*1			#Compute gradient of bias term

				#for each gate
				for(j in 33:1){
					#rebuild original circuit
					m <- ckt()
					#Apply input to compare to modified versions
					p <- qapp(x,m)
					Ep <- prob1(p)				#Get probability of measuring 1
					px <- Ep + bias				#Above formula is for expectation [-1,1], if using prob, its just + bias
				
					#alpha
					m2 <- ckt("a",1,j)	#get circuit with gate replace by da
					pa <- qapp(x,m2)	#apply circuit to get d
					dC_da[j] <- cmpr(p,pa)
					if(isControlled[j]){						#if controlled
						m2 <- ckt("a",1,j,TRUE)					#do again with inverted gate
						pa <- qapp(x,m2)
						dC_da[j] <- 1/2*dC_da[j] - 1/2*cmpr(p,pa)		#take difference
					}
					dC_da[j] <- dC_da[j] * (px - y)				#find dC_da by adding desired direction
					alpha[j] <- alpha[j] - eta*dC_da[j]			#update alpha for gate j
		
					#beta
					m2 <- ckt("b",1,j)
					pb <- qapp(x,m2)
					m2 <- ckt("b",2,j)
					pb2 <- qapp(x,m2)
					dC_db[j] <- 1/2*(cmpr(p,pb)+cmpr(p,pb2))
					if(isControlled[j]){
						m2 <- ckt("b",1,j,TRUE)
						pb <- qapp(x,m2)
						m2 <- ckt("b",2,j,TRUE)
						pb2 <- qapp(x,m2)
						dC_db2 <- 1/2*(cmpr(p,pb)+cmpr(p,pb2))
						dC_db[j] <- 1/2*dC_db[j] - 1/2*dC_db2
					}
					dC_db[j] <- dC_db[j] * (px - y)				#find dC_db by adding desired direction
					beta[j] <- beta[j] - eta*dC_db[j]			#update beta for gate j

					#gamma
					m2 <- ckt("g",1,j)
					pg <- qapp(x,m2)
					m2 <- ckt("g",2,j)
					pg2 <- qapp(x,m2)
					dC_dg[j] <- 1/2*(cmpr(p,pg)+cmpr(p,pg2))
					if(isControlled[j]){
						m2 <- ckt("g",1,j,TRUE)
						pg <- qapp(x,m2)
						m2 <- ckt("g",2,j,TRUE)
						pg2 <- qapp(x,m2)
						dC_dg2 <- 1/2*(cmpr(p,pg)+cmpr(p,pg2))
						dC_dg[j] <- 1/2*dC_dg[j] - 1/2*dC_dg2
					}
					dC_dg[j] <- dC_dg[j] * (px - y)				#find dC_dg by adding desired direction
					gamma[j] <- gamma[j] - eta*dC_dg[j]			#update gamma for gate j
		
				}
				#Update Network from computed gradients
				#alpha <- alpha - eta*dC_da				#Now happen after each gate
				#beta <- beta - eta*dC_db				#instead of all at once here
				#gamma <- gamma - eta*dC_dg		
				bias <- bias - eta*dC_dbias		*bsc	#update bias after all gates have been updated

				#Perform learning rate decay (if applied)
				eta <- decay * eta

				#Normalize Data
				pix[1,tr] <- pix[1,tr]/(N-Nt)			#N-Nt digits other than target
				pix[2,tr] <- pix[2,tr]/Nt				#Nt target digits
				p1[1,tr] <- p1[1,tr]/(N-Nt)			
				p1[2,tr] <- p1[2,tr]/Nt				
			}
		}
	}

	#Run Test (regardless of whether network was trained or not)
	for(j in 1:33){
		g[[j]] <- G(alpha[j],beta[j],gamma[j])
		if(!unitary(g[[j]])){
			write(paste("Error: Gate",j,"not unitary. Returning alpha,beta,gamma,bias"),file=filename,append=TRUE)
			return(list(alpha,beta,gamma,bias))
		}
	}
	m <- ckt()					#generate circuit
	networkTest(m)				#run test
				
	
	#Plot output if the network was trained
	if(train & pl){
		jpeg(paste(imagename,".jpg"))
		plot(seq(1,t,by=1),pix[2,],ylim=c(-1,2),type="l",lwd=3,col="Blue")
		lines(seq(1,t,by=1),pix[1,],lwd=3,col="Red")
		lines(seq(1,t,by=1),p1[2,],lwd=3,col="Purple")
		lines(seq(1,t,by=1),p1[1,],lwd=3,col="Orange")
		legend("topright",legend=c("Network Output (target)","Network Output (non-target)","Prob meas 1 (target)","Prob meas 1 (non-target)"),fill=c("Blue","Red","Purple","Orange"))
		dev.off()
	}
	
	list(g,m,alpha,beta,gamma)
}











