

QuantumClassifier <- function(	n=8,B=2,r=c(1,3),
			data=NULL,labels=NULL,digit=0,
			eta=1,decay=1,bsc=1,t=20,tag="",pl=TRUE,train=TRUE,
			validT=FALSE,vdata=NULL,vlabels=NULL,
			pretrained=FALSE,alpha=NULL,beta=NULL,gamma=NULL,bias=NULL,
		       	writeParameters=FALSE,outputPath=NULL	){

	filename <- "QuantumClassifierOutput"
	imagename <- "QuantumClassifierProbabilties"
	if(tag != ""){
		filename <- paste(filename,"_",tag,sep="")
		imagename <- paste(imagename,"_",tag,sep="")
	}

	#Since there are n qubits, there are n single qubit gates and n/gcd(n,r) controlled gates per block
	if( B != length(r) )
		print("Warning: number of blocks specified (B) is not equal to length of ranges specified (r)")
	nControlledGates <- floor(n/gcd(n,r))
	GatesBeforeBlock <- c(0,cumsum(nControlledGates+n))
	Ngates <- n*B + sum(nControlledGates) + 1	#and one at the end
	

	#Need alpha, beta, and gamma for each gate
	if(!pretrained){						#Create vector of all parameters for each type
		alpha <- runif(Ngates,0,2*pi)
		beta <- runif(Ngates,0,2*pi)
		gamma <- runif(Ngates,0,2*pi)
		bias <- 0.0
	}
		
	#Vectors for gradients
	dC_da <- rep(0,Ngates)
	dC_db <- rep(0,Ngates)
	dC_dg <- rep(0,Ngates)
	dC_dbias <- 0
	
	#list of gates
	g <- vector("list",Ngates)

	#Circuit building function
	#param will set a gate to derivative wrt to that parameter
	#which indicates whether its the first or 2nd term (relevant for beta and gamma)
	#gateNo is the gate identifier
	#invert flips the sign (relevant for controlled gates)
	ckt <- function(param="",which=1,gateNo=0,invert=FALSE,byCycle=FALSE,byGateCycle=FALSE){

		#copy set of gates to temproray gate set
		gc <- g
		if(param == "a"){	#replace 1 gate with derivative wrt alpha
			gc[[gateNo]] <- G(alpha[gateNo]+pi/2,beta[gateNo],gamma[gateNo])		
		} else if(param == "b"){ 	#replace 1 gate with derivatie wrt beta
			if(which == 1){
				gc[[gateNo]] <- G(alpha[gateNo],beta[gateNo]+pi/2,0)				
			} else{
				gc[[gateNo]] <- G(alpha[gateNo],beta[gateNo]+pi/2,pi)
			}
		} else if(param == "g"){	#replace 1 gate with derivatie wrt gamma
			if(which == 1){
				gc[[gateNo]] <- G(alpha[gateNo],0,gamma[gateNo]+pi/2)				
			} else{
				gc[[gateNo]] <- G(alpha[gateNo],pi,gamma[gateNo]+pi/2)
			}
		}
		if(param != "" && invert)				#Invert (for some controlled gates)
			gc[[gateNo]] <- -1*gc[[gateNo]]	

		#Generate circuit matrix
		M <- diag(2^n)				#start with identity matrix for n qubits
		ckt_ByCycle <- list()			#If getting the circuit (full matrix for each cycle)
		#This is how gates will be reported to the Gate Decomposition
		gates_ByCycle <- list()			#There is a list, each entry of which is for each cycle of the circuit
		#Each entry of the list, will be another list, which has vectors as elements
		#The vectors contain [ 1 or 2 (1/2 qubit gate) , control qubit , target qubit ]
		#alpha,beta,gamma for each gate should be stored externally, use those

		#For each block
		for(b in 1:B){
			idx <- GatesBeforeBlock[b] + 1		#Each block has some number of gates (R indexes from 1)
			#Build initial parallel single qubit gates
			m <- gc[[idx]]								#gate on qubit 0
			gateCycle <- list(c(1,-1,0))	#Single qubit gate, on qubit 0
			for(j in 2:n){
				m <- tensor(m,gc[[idx+j-1]])			#gates on qubits 1-n
				gateCycle <- c(gateCycle, list( c(1,-1,j-1) ))
			}

			M <- m %*% M								#Add to M
			ckt_ByCycle <- c( ckt_ByCycle , list(m) )				#And also to list
			gates_ByCycle <- c(gates_ByCycle, list(gateCycle))

			#Now do n controlled gates
			j <- 1:nControlledGates[b]
			target <- rev((j*r[b] - r[b] ) %% n)		#Target determined by (r)ange for this (b)lock
			control <- rev((j*r[b]) %% n)		#Same for control
			for(j in 0:(nControlledGates[b]-1)){	
				m <- cntrld(gate=gc[[idx+n+j]] , n=n, control[j+1], target[j+1])
				M <- m %*% M
				ckt_ByCycle <- c( ckt_ByCycle , list(m) )
				gates_ByCycle <- c( gates_ByCycle, list(list(c(2,control[j+1],target[j+1]))) )	#Double list to match single
			}
		}
		
		#One last gate on quibt 0
		m <- single(gate=gc[[Ngates]], n=n, t=0)
		M <- m %*% M
		ckt_ByCycle <- c( ckt_ByCycle , list(m) )
		gates_ByCycle <- c(gates_ByCycle, list(list( c(1,-1,0) )))	
	
		#Return M
		if(byCycle == FALSE){
			M
		} else{
			if(!byGateCycle){
				ckt_ByCycle
			}else{
				gates_ByCycle
			}
		}
	}

	#apply circuit function
	qapp <- function(v,M){
		M %*% v
	}

	#probability of measuring 1 on first qubit
	prob1 <- function(v){
		sum(probs(v)[ (2^(n-1)+1) : 2^n ])		#The 2nd half of the probability vector
	}

	#Comparison	Re{ <Ut x | z | Ux> }
	Zop <- tensor( Z() , diag(2^(n-1)) )		#Z measurement on first qubit
	cmpr <- function(v,w){
		Re(adjoint(v) %*% Zop %*% w)
	}

	#Run Network on training or validation  set
	networkTest <- function(M,d="Train"){
		if(d == "Train"){
			tdata <- data
			tlabels <- labels 
		}else if(d == "Test"){
			tdata <- vdata
			tlabels <- vlabels
		}
		N <- dim(tdata)[1]							#number of samples in validation set		
		p <- rep(0,N)								#probability of measuring correct result
		r <- rep(0,N)								#if probability of correct result is higher than 50%
		probs <- rep(0,N)
		for(j in 1:N){								#for each validation input
			v <- do.call(ket,as.list(tdata[j,]))	#create ket of input
			v <- qapp(v,M)							#apply the circuit
			pr1 <- prob1(v)						#probability of measuring 1
			p[j] <- pr1
			probs[j] <- pr1
			#target <- 1
			#if( tlabels[j] != digit){
			#	p[j] <- 1 - p[j]					#flip if target digit is different than current input
			#	target <- 0
			#}
			#if( p[j] + bias > 0.5)
			#	r[j] <- 1

			if( tlabels[j] != digit){
				target <- 0
				if(p[j] + bias < 0.5)
					r[j] <- 1
				p[j] <- 1 - (p[j] + bias)
			}else{
				target <- 1
				if(p[j] + bias >= 0.5)
					r[j] <- 1
				p[j] <- p[j] + bias
			}

			#pp("Target: ",tlabels[j],"->",target," Prob1: ",pr1)
		}
		pp("On",length(p),"inputs")
		pp("The final bias value is ",bias)
		write(pp("With a single measurement:    Network achieves ",mean(p),"accuracy on ",d," set"),file=filename,append=TRUE)
		write(pp("With a repeated measurements: Network achieves ",mean(r),"accuracy on ",d," set"),file=filename,append=TRUE)
		digitIdx <- which(tlabels == digit)
		write(pp("For target digit: P1: ",mean(probs[digitIdx]),"  NetOut: ",mean(probs[digitIdx])+bias,"  Repeated Accuracy: ",mean(r[digitIdx])))
		write(pp("For other digits: P1: ",mean(probs[-digitIdx]),"  NetOut: ",mean(probs[-digitIdx])+bias,"  Repeated Accuracy: ",mean(r[-digitIdx])))
	}
	#Run network on validation input set
	networkValidate <- function(M){
		networkTest(M,"Test")
	}


	
	if(train){
		N <- length(labels)		#N is the number of samples provided in training set
		Nt <- sum(labels == digit)	#Nt is the number of samples that are the target digit
	
		#Rows are different digits, target and sum of all rest
		#t is number of training iterations, keeping track of accuracy over iterations
		pix <- matrix(rep(0,2*t),nrow=2)		#pi(x;theta,b) = output of network
		p1 <- matrix(rep(0,2*t),nrow=2)		#probability of measuring 1 on output qubit

		#Know which gates are controlled gates, for training purposes
		#B sequences of n FALSE and then n TRUE, followed by FALSE
		isControlled <- c( rep( c( rep(FALSE,n),rep(TRUE,n) ) , B ) , FALSE )

		for(tr in 1:t){
			print(paste("Iteration",tr,"of",t))


			#For each sample provided
			for(s in 1:N){
				print("")
				cat(paste("Sample",s))

				#Set up input (need to check if power of n)
				x <- do.call(ket,as.list(data[s,]))	#encode input integers as the amplitudes of a ket
				#and target
				y <- 0						#0 for all digits
				if(labels[s] == digit)		#except target
					y <- 1

				#Build gates
				for(j in 1:Ngates){
					g[[j]] <- G(alpha[j],beta[j],gamma[j])
					if(!unitary(g[[j]])){
						write(paste("Error: Gate",j,"not unitary. Returning alpha,beta,gamma,bias"),file=filename,append=TRUE)
						return(list(alpha,beta,gamma,bias))
					}
				}

				#get "normal" circuit
				M <- ckt()

				#Do a validation run
	
				if(validT && s==1)
					networkValidate(M)
				
				#Apply input
				p <- qapp(x,M)

				#Get probability and network output
				Ep <- prob1(p)				#Prop 1
				px <- Ep + bias				#Network output

				#Log results
				p1[y+1,tr] <- p1[y+1,tr] + Ep		#Record for overall training graph
				pix[y+1,tr] <- pix[y+1,tr] + px		#normalize later
				write(paste("px:",px,"   Prob 1:",Ep," bias:",bias," input:",labels[s]," target:",y),file=filename,append=TRUE)

				#Compute gradients and update parameters
				
				if( abs(bias) < 0.15 ){	#Prevent bias growing without bound
					dC_dbias <- (px-y)*1						#Compute gradient of bias term
					bias <- bias + eta*dC_dbias		*bsc		#Update 
				}

				#For each gate
				cat(paste("Computing gradients for",Ngates,"gates:"))
				for(j in Ngates:1){
					cat(".")

					#rebuild original circuit
					M <- ckt()

					#Apply input to compare to modified versions
					p <- qapp(x,M)

					Ep <- prob1(p)				#Get probability of measuring 1
					px <- Ep + bias	

					#alpha
					M2 <- ckt("a",1,j)		#get circuit with gate replaced by da
					pa <- qapp(x,M2)		#apply circuit
					daG <- cmpr(p,pa)	#get gradient
					if(isControlled[j]){	#if controlled
						M2 <- ckt("a",1,j,TRUE)	#do again w/ inverted gate
						pa <- qapp(x,M2)
						daG <- 1/2*daG - 1/2*cmpr(p,pa)	#combine w/ difference
					}
					dC_da[j] <- daG * (px - y)	#find dC_da by adding desired directions
					alpha[j] <- alpha[j] + eta*dC_da[j]

					#beta
					M2 <- ckt("b",1,j)
					pb <- qapp(x,M2)
					M2 <- ckt("b",2,j)
					pb2 <- qapp(x,M2)
					dbG <- 1/2*(cmpr(p,pb)+cmpr(p,pb2))
					if(isControlled[j]){
						M2 <- ckt("b",1,j,TRUE)
						pb <- qapp(x,M2)
						M2 <- ckt("b",2,j,TRUE)
						pb2 <- qapp(x,M2)
						dbG2 <- 1/2*(cmpr(p,pb)+cmpr(p,pb2))
						dbG <- 1/2*dbG - 1/2*dbG2
					}
					dC_db[j] <- dbG * (px - y)
					beta[j] <- beta[j] + eta*dC_db[j]

					#beta
					M2 <- ckt("g",1,j)
					pg <- qapp(x,M2)
					M2 <- ckt("g",2,j)
					pg2 <- qapp(x,M2)
					dgG <- 1/2*(cmpr(p,pg)+cmpr(p,pg2))
					if(isControlled[j]){
						M2 <- ckt("g",1,j,TRUE)
						pg <- qapp(x,M2)
						M2 <- ckt("g",2,j,TRUE)
						pg2 <- qapp(x,M2)
						dgG2 <- 1/2*(cmpr(p,pg)+cmpr(p,pg2))
						dgG <- 1/2*dgG - 1/2*dgG2
					}
					dC_dg[j] <- dgG * (px - y)
					gamma[j] <- beta[j] + eta*dC_dg[j]

				}

				#Perform learning rate decay (if applied)
				eta <- decay * eta

				#Normalize Data
				pix[1,tr] <- pix[1,tr]/(N-Nt)			#N-Nt digits other than target
				pix[2,tr] <- pix[2,tr]/Nt				#Nt target digits
				p1[1,tr] <- p1[1,tr]/(N-Nt)			
				p1[2,tr] <- p1[2,tr]/Nt	
			}
			#If writing the output parameters as training happens
			if(writeParameters){
				#Write parameters to file for continuing later
				pdir <- paste(outputPath,tag,"/",(tr+1) %% 2,"/",sep="")
				if( !dir.exists(pdir) )
					dir.create(pdir)
				write(alpha,file=paste(pdir,"alpha",sep=""),ncolumns=1)
				write(beta,file=paste(pdir,"beta",sep=""),ncolumns=1)
				write(gamma,file=paste(pdir,"gamma",sep=""),ncolumns=1)
				write(bias,file=paste(pdir,"bias",sep=""))
				CKTl <- ckt(byCycle=TRUE)
				for(j in 1:length(CKTl))
					write(CKTl[[j]],file=paste(pdir,"m",j,sep=""),ncolumns=1)
			}
		}
	}
	#Finish training

	#Run Test (regardless of whether network was trained or not)
	for(j in 1:Ngates){
		g[[j]] <- G(alpha[j],beta[j],gamma[j])
		if(!unitary(g[[j]])){
			write(paste("Error: Gate",j,"not unitary. Returning alpha,beta,gamma,bias"),file=filename,append=TRUE)
			return(list(alpha,beta,gamma,bias))
		}
	}
	M <- ckt()					#generate circuit
	networkTest(M)				#run test
	networkValidate(M)
				
	
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
	
	list(g,M,alpha,beta,gamma,ckt(byCycle=TRUE),ckt(byCycle=TRUE,byGateCycle=TRUE))
}




















