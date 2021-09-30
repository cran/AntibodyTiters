\name{plotAllABT}
\alias{plotAllABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Plotting antibody titers of All patients.
}
\description{
    This function plots antibody titers of all the patients 
    in the ABT class object produced by \code{\link{readABT}}.
}
\usage{
plotAllABT(objName = "inData", prefix = "", 
    dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = FALSE, ylab = "Titer (AU)", savePDF = FALSE, 
    alphaFactor = 10, lwd = 2)
}

\arguments{
    \item{objName}{a character specifying the name of an ABT class object produced by readABT.}
    \item{prefix}{a character to be atattched to the file name.}
    \item{dayStart}{an integer specifying the start day, from which scores are plotted.}
    \item{dayEnd}{an integer specifying the end day, to which scores are plotted.}
    \item{type}{a character determining the time scale; "weeks" or "days".}
    \item{rainbow}{a logical indicating wether the line color is rainbow.}
    \item{ylab}{a character for the y-axix label.}
    \item{savePDF}{a logical whether a PDF file is stored in the current directory.}
    \item{alphaFactor}{an integer for controlling the transparency of lines.}
    \item{lwd}{an integer for controlling the width of lines.}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = TRUE, savePDF = FALSE)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = TRUE, savePDF = FALSE, alphaFactor = 5)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = TRUE, savePDF = FALSE, alphaFactor = 20)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = TRUE, savePDF = FALSE, lwd = 4)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = FALSE, savePDF = FALSE)
plotAllABT(objName = "inData", dayStart = 140, dayEnd = 350, 
    type = "weeks", rainbow = TRUE, savePDF = FALSE)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "days", rainbow = TRUE, savePDF = FALSE)
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "days", rainbow = FALSE, savePDF = FALSE)
plotAllABT(objName = "inData", dayStart = 140, dayEnd = 350, 
    type = "days", rainbow = TRUE, savePDF = FALSE)
}

\value{
No object will be returned. Instead, this function will drow a plot in a graphic device, or save a PDF file in the current directory.
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

