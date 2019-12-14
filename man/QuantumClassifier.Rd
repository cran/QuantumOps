\name{QuantumClassifier}
\alias{QuantumClassifier}
\title{QuantumClassifier}
\usage{
QuantumClassifier(	n=8,B=2,r=c(1,3),
			data=NULL,labels=NULL,digit=0,
			eta=1,decay=1,bsc=1,t=20,tag="",pl=TRUE,train=TRUE,
			validT=FALSE,vdata=NULL,vlabels=NULL,
			pretrained=FALSE,alpha=NULL,beta=NULL,gamma=NULL,bias=NULL,
		       	writeParameters=FALSE,outputPath=NULL	)
}
\arguments{
  \item{n}{Number of qubits that this the classifier will use. }

  \item{B}{Number of blocks in the circuit. A block consists of n single qubit gates applied to each of the qubits in parallel and then n/(gcd,r) controlled qubit gates to perform a maximal entanglement. r is the specified range of the controlled gates}

  \item{r}{Vector containing the range for each block. length(r) should be equal to B. r of 1 means controlled gates are performed on adjacent qubits}

  \item{data}{ matrix containing input training data. Rows are individual samples. The number of columns should be equal to 2^n }

  \item{labels}{ Vector containing labels of digits. Length must be the same as the number of rows in data }

  \item{digit}{ Individual output to identify. The network will attempt to differentiate between inputs that are labelled as digit (in the labels vector) and inputs that are labelled as any other number }

  \item{eta}{ learning rate for parameter updates }

  \item{decay}{ Multiplier for learning rate after each training iteration. If set to less than 1, the learning rate decays in time }

  \item{bsc}{ Scaler for the learning rate of the bias. Setting to a low value will result in other parameters updating faster than the bias }

  \item{t}{ Number of training iterations to perform. Total runs is equal to this value multiplied by the number of samples provided }

  \item{tag}{ String to attach to name of output files}

  \item{pl}{Boolean indicating whether training output should be plotted}

  \item{train}{Boolean specifying if network should trained on training data, only false if passing in pretrained model}

  \item{validT}{Boolean specifying if the network should be tested on validation data while being trained}

  \item{vdata}{Validation data, necessary if validT is set to true in which case network is tested on this data while being trained. Can be set to same as data.}

  \item{vlabels}{Validation labels, necessary if validT is set to true in which case network is tested on vdata while being trained. Can be set to same as labels.}

  \item{pretrained}{Boolean specifying if a pretrained model is being passed in. If so, alpha, beta, and gamme will be set to intputs, rather than randomized.}

  \item{alpha}{alpha values for gates if pretrained is set to TRUE, should be a vector of length equal to the number of gates in the circuit.}

  \item{beta}{beta values for gates if pretrained is set to TRUE, should be a vector of length equal to the number of gates in the circuit.}

  \item{gamma}{gamma values for gates if pretrained is set to TRUE, should be a vector of length equal to the number of gates in the circuit. }

  \item{bias}{Bias applied to the output of the circuit.}

  \item{writeParameters}{ Boolean specifying whether function should write the parameters as it trains. Useful when training takes a long time.}

  \item{outputPath}{String which specifies path to write output parameters to if writeParameters is TRUE. Must have write priveleges in this directory. The function will create two directories inside outputPath, named 0 and 1, and will alternate output to each folder. This prevents corruption of output if interrupted.}
}

\value{
List containing a list of the 33 gates and the matrix representing the entire circuit of the trained classifier
}

\description{
Quantum classifier which was proposed by Maria Schuld (2018). Consists of code blocks which have parallel single qubit quantum gates followed by controlled qubit gates. Takes as input samples and a corresponding list of labels indicating the correct output value of each sample. Will update the parameters of the gates in order to correctly identify the samples. 
}

\examples{
 \dontrun{
 QuantumClassifier(n=8,B=2,r=c(1,3),
    matrix(sample(256,replace=TRUE),nrow=1),
	array(1),0,1,1,.001,1,"test")
 }
}
