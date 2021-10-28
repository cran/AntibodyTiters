\name{groupAttribABT}
\alias{groupAttribABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Grouping of patients by scores.
}
\description{
    Addition of a new attribute that categorizes patients in specified groups to an ABT class object. 
}
\usage{
groupAttribABT(objName = "inData", sourceAttrib = "Age", 
    newAttribName = "AgeGroup", groupNames = c("young", "middle", "elderly"),
    groupLimmits = list(c(0, 39), c(40, 64), c(65, 200)))
}
\arguments{
    \item{objName}{a character specifying the name of an ABT class object produced by readABT.}
    \item{sourceAttrib}{a character specifying the name of source attribute. The contents of
    	this attribute must be integer of numeric.}
    \item{newAttribName}{a character specifying the name of the new attribute.}
    \item{groupNames}{a character vector specifying the names of categories.}
    \item{groupLimmits}{a list of two value integers specifying the lower and upper limmits of scores. 
    	The order of values in this list must correspond to the order of values in \code{groupNames}}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))
inDataA <- groupAttribABT(objName = "inData", sourceAttrib = "Age", 
    newAttribName = "AgeGroup", groupNames = c("young", "middle", "elderly"),
    groupLimmits = list(c(0, 39), c(40, 64), c(65, 200)))
plotAllABT("inDataA", lineAttrib = "AgeGroup", addPoints = TRUE, savePDF = FALSE, 
    orderOfCategories = c("young", "middle", "elderly"), type = "weeks")
plotAllABT("inDataA", lineAttrib = "AgeGroup", addPoints = TRUE, savePDF = FALSE, 
    orderOfCategories = c("young", "middle", "elderly"), type = "M1", geometricMean = TRUE)
}

\value{
an ABT class object
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

