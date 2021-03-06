\name{QuantumMNIST256Classifier}
\alias{QuantumMNIST256Classifier}
\title{QuantumMNIST256Classifier}
\usage{
QuantumMNIST256Classifier(
	data=NULL,labels=NULL,digit=0,
	eta=1,decay=1,bsc=1,t=20,tag="",pl=TRUE,train=TRUE,
	validT=FALSE,vdata=NULL,vlabels=NULL,
	pretrained=FALSE,alpha=NULL,beta=NULL,gamma=NULL)
}
\arguments{
  \item{data}{ matrix containing input training data. Rows are individual samples. There must be 256 columns }

  \item{labels}{ Vector containing labels of digits. Length must be the same as the number of rows in data }

  \item{digit}{ Individual digit (0-9) to identify }

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

  \item{alpha}{alpha values for gates if pretrained is set to TRUE, should be a vector of length 33}

  \item{beta}{beta values for gates if pretrained is set to TRUE, should be a vector of length 33}

  \item{gamma}{gamma values for gates if pretrained is set to TRUE, should be a vector of length 33}
}

\value{
List containing a list of the 33 gates and the matrix representing the entire circuit of the trained classifier
}

\description{
Quantum classifier which was proposed by Maria Schuld (2018). Consists of 33 quantum gates with a depth of 19. Takes as input samples with dimensions of 256 and a corresponding list of labels indicating the correct output value of each sample. Will update the parameters of the gates in order to correctly identify one of the digits specified. 
}

\examples{
 \dontrun{
 QuantumMNIST256Classifier(matrix(sample(256,replace=TRUE),nrow=1),array(1),0,1,1,.001,1,"test")
 }
}
