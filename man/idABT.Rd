\name{idABT}
\alias{idABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Extracting patient IDs from an ABT class object.
}
\description{
    With this function, patient IDs in the ABT class object 
    can be extracted.
}
\usage{idABT(objName = "inData")}

\arguments{
    \item{objName}{a character specifying the name of an ABT class object produced by readABT.}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))
idABT(objName = "inData")[1:6]
}

\value{
a character vector
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

