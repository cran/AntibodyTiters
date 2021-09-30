\name{plotEachABT}
\alias{plotEachABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Plotting antibody titers of each patient.
}
\description{
    This function plots antibody titers of one specified patient 
    in the ABT class object produced by \code{\link{readABT}}.
}
\usage{
plotEachABT(patientID = "patient A", objName = "inData", 
    prefix = "", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", ylab = "Titer (AU)", savePDF = FALSE, addPoints = FALSE, lwd = 2)
}

\arguments{
    \item{patientID}{a character specifying the name of an ABT class object produced by readABT.}
    \item{objName}{an ABT class object produced by readABT.}
    \item{prefix}{a character to be atattched to the file name.}
    \item{dayStart}{an integer specifying the start day, from which scores are plotted.}
    \item{dayEnd}{an integer specifying the end day, to which scores are plotted.}
    \item{type}{a character determining the time scale; "weeks" or "days".}
    \item{ylab}{a character for the y-axix label.}
    \item{savePDF}{a logical indicating whether a PDF file is stored in the current directory.}
    \item{addPoints}{a logical indicating whether points are added.}
    \item{lwd}{an integer for controlling the width of lines.}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks", savePDF = FALSE)
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks", savePDF = FALSE, 
    lwd = 2)
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks", savePDF = FALSE, 
    addPoints = TRUE)
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "days", savePDF = FALSE)
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = 140, dayEnd = 350, type = "weeks", savePDF = FALSE)
plotEachABT(patientID = "patient 2", objName = "inData", 
    dayStart = 140, dayEnd = 350, type = "days", savePDF = FALSE)
}

\value{
No object will be returned. Instead, this function will drow a plot in a graphic device, or save a PDF file in the current directory.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

