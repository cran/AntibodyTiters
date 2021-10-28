\name{readABT}
\alias{readABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Reading an xlsx file 
    and examining the validity of the dataset.
}
\description{
    With this function, a specifically formatted xlsx file 
    can be loaded. After validation of the file, 
    'pmax' and other key values were examined for 
    the following analysis. The output is an ABT object. 
    An object of this class is a list with four contents named 
    \code{DATA}, \code{pmax}, \code{longestFromSecond} and 
    \code{shortestFromSecond}. \code{DATA} is a data.frame. 
    The other three are integer objects. 
    These values are used for the plotting functions 
    \link{plotAllABT} and \link{plotEachABT}.
}
\usage{readABT(fileName = "xxx.xlsx", attribNumeric = "Age")}

\arguments{
    \item{fileName}{a file name to be read.}
    \item{attribNumeric}{a character vector specifying attribute columns to have numeric values.}
}

\examples{
# inData <- readABT(fileName = "xxx.xlsx")
}

\value{
an ABT-class object: see the Description above for the detail of this class.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

