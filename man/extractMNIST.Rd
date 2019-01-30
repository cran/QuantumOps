\name{extractMNIST}
\alias{extractMNIST}
\title{extractMNIST}
\usage{
extractMNIST(data,labels,s,centercrop=TRUE)
}
\arguments{
  \item{data}{ String of path to file containing MNIST training images   }

  \item{labels}{ String of path to file containing MNIST training labels }
 
  \item{s}{ Number of samples and labels to extract from file}

  \item{centercrop}{ Boolean indicating whether the images should be centercropped to contain only 256 points }
}

\value{
List containing matrix of image data and array of training labels
}

\description{
Opens the MNIST training data and label files (not provided with package) and extracts the images and labels and returns them in a list
}

\examples{
 \dontrun{
 extractMNIST("train-images.idx3-ubyte","train-labels.idx1-ubyte",2)
 }
}
