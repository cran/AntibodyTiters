\name{toyABT}
\alias{toyABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Production of an xlsx file with toy data.
}
\description{
    \pkg{AntibodyTiters} requires the input xlsx files be 
    in a specific format. This function produces a toy 
    xlsx file in the format.
}
\usage{
toyABT(fileName = "toy.xlsx", pmax = 7, patients = 20, 
    outsiderPercent = 2, NaPercent = 10, 
	Attrib = c("Sex", "Age", "VeryLow"), 
	attribFactors = list(c("F", "M"), c(18, 80), c(TRUE, FALSE)))
	}

\arguments{
    \item{fileName}{a file name to be produced.}
    \item{pmax}{an integer (>=4 & <18) for specifying 
    	the maximam sampling point.}
    \item{patients}{an integer specifying the number of patients.}
    \item{outsiderPercent}{an integer specifying the percentage 
    	of patients with outsider values.}
    \item{NaPercent}{an integer specifying the percentage 
    	of patients with NA values.}
    \item{Attrib}{a character vector for the names of Attrib.}
    \item{attribFactors}{a list determining the values of Attrib. 
    	If the names of Attrib contained "Age", 
    	two numbers provided here are used as lower and upper limits.}
}

\examples{
# toyABT(fileName = "toy.xlsx", pmax = 9, patients = 120, 
# 	outsiderPercent = 2, NaPercent = 10, 
# 	Attrib = c("Sex", "Age", "VeryLow"), 
# 	attribFactors = list(c("F", "M"), c(18, 80), c(TRUE, FALSE)))
# The above command will create an excel file with toy data in the current directory. 
}

\value{
An xlsx file will be stored in the current directory. 
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

