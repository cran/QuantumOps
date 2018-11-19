i <- complex(1,0,1)									#Convenient to use i
I <- matrix(c(1,0,0,1),nrow=2,ncol=2)
H <- 1/2^.5 * matrix(c(1,1,1,-1),nrow=2,ncol=2)		#H gate
X <- matrix(c(0,1,1,0),nrow=2,ncol=2)				#X gate
Y <- matrix(c(0,i,-i,0),nrow=2,ncol=2)				#Y gate
Z <- matrix(c(1,0,0,-1),nrow=2,ncol=2)				#Z gate
I <- matrix(c(1,0,0,1),nrow=2,ncol=2)
S <- matrix(c(1,0,0,i),nrow=2,ncol=2)
T <- matrix(c(1,0,0,exp(i*pi/4)),nrow=2,ncol=2)


CNOT <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,0,1, 0,0,1,0),nrow=4,ncol=4)

zp <- ket(1,0) 
zm <- ket(0,1)
xp <- ket(1,1)
xm <- ket(1,-1)
yp <- ket(1,i)
ym <- ket(1,-i)
B00 <- ket(1,0,0,1)
B01 <- ket(0,1,1,0)
B10 <- ket(1,0,0,-1)
B11 <- ket(0,1,-1,0)
BELL <- CNOT %*% tensor(H,I)
