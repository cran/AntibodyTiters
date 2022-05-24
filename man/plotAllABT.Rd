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
    type = "weeks", rainbow = FALSE, ylab = "Titer (AU/ml)", savePDF = FALSE, 
    alphaFactor = 10, lwd = 2, lineAttrib = "", addPoints = FALSE, 
    orderOfCategories = "", lowessSmooth = FALSE, geometricMean = FALSE,
    lineForPre = FALSE, lineFor1st = FALSE, logY = TRUE, PDFwidth = 8, PDFheight = 5, 
    main = NULL, omitPreVac = FALSE, lineColDark = FALSE)
}

\arguments{
    \item{objName}{a character specifying the name of an ABT class object produced by readABT.}
    \item{prefix}{a character to be atattched to the file name. This is active when \code{savePDF==TRUE}.}
    \item{dayStart}{an integer specifying the start day, from which scores are plotted.}
    \item{dayEnd}{an integer specifying the end day, to which scores are plotted.}
    \item{type}{a character determining the time scale; "weeks", "days", "M1", "M2", "M3", "M4", "M6". 
        When "weeks" or "days" is chosen, plotting will be done on a continuous time scale. 
        In contrast, when "M1" or other "M-" is chosen, time scale will be discrete. 
        Bin sizes are one month (M1), two (M2), three (M3), four (M4), and six (M6) months.}
    \item{rainbow}{a logical indicating wether the line color is rainbow.}
    \item{ylab}{a character for the y-axix label.}
    \item{savePDF}{a logical indicating wether a PDF file is stored in the current directory.}
    \item{alphaFactor}{an integer for controlling the transparency of lines.}
    \item{lwd}{an integer for controlling the width of lines.}
    \item{lineAttrib}{a character specifying the name of attribute for which lowess or geometric mean lines are drawn for each group.}
    \item{addPoints}{a logical indicating wether points are plotted. This is active when \code{lineAttrib!=""}.}
    \item{orderOfCategories}{a character vector specifying the order of categories listed as legends. This is active when \code{lineAttrib!=""}.}
    \item{lowessSmooth}{a logical indicating wether lowess lines are plotted. This is active when \code{lineAttrib!=""}.}
    \item{geometricMean}{a logical indicating wether geometric mean lines are plotted. This is active when \code{lineAttrib!=""}.}
    \item{lineForPre}{a logical indicating wether lines that connects the pre-vaccination sampling point to others are plotted. This is active when \code{lineAttrib!="" & geometricMean==TRUE}.}
    \item{lineFor1st}{a logical indicating wether lines that connects the after-1st shot sampling point to others are plotted. This is active when \code{lineAttrib!="" & geometricMean==TRUE}.}
    \item{logY}{a logical indicating whether Y-axis is in log scale.}
    \item{PDFwidth}{an integer specifying the width of PDF. This is active when \code{savePDF==TRUE}.}
    \item{PDFheight}{an integer specifying the height of PDF. This is active when \code{savePDF==TRUE}.}
    \item{main}{a character for the main title. When not specified, \code{objName} will be used as the title.}
    \item{omitPreVac}{a logical indicating whether pre-vaccination will be omitted.}
    \item{lineColDark}{a logical indicating whether line colors will be darkened.}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))

# logY
plotAllABT(objName = "inData", type = "weeks", rainbow = TRUE, logY = TRUE)
plotAllABT(objName = "inData", type = "weeks", rainbow = TRUE, logY = FALSE)

# lineAttrib, lowessSmooth, addPoints
plotAllABT(objName = "inData", type = "weeks", rainbow = FALSE, 
    lineAttrib = "Sex")
plotAllABT(objName = "inData", type = "weeks", rainbow = FALSE, 
    lineAttrib = "Sex", lowessSmooth = TRUE)
plotAllABT(objName = "inData", type = "weeks", rainbow = FALSE, 
    lineAttrib = "Sex", lowessSmooth = TRUE, addPoints = TRUE)
plotAllABT(objName = "inData", type = "weeks", rainbow = FALSE, 
    lineAttrib = "VeryLow", lowessSmooth = TRUE, addPoints = TRUE)
    
# Discrete mode, geometricMean
plotAllABT(objName = "inData", 
    type = "M1", lineAttrib = "Sex", geometricMean = TRUE)
plotAllABT(objName = "inData", 
    type = "M3", lineAttrib = "Sex", geometricMean = TRUE)

# alphaFactor, lwd
plotAllABT(objName = "inData", type = "weeks", rainbow = TRUE)
plotAllABT(objName = "inData", type = "weeks", rainbow = TRUE, alphaFactor = 20)
plotAllABT(objName = "inData", type = "weeks", rainbow = TRUE, lwd = 4)

# dayStart, dayEnd
plotAllABT(objName = "inData", dayStart = as.integer(NA), dayEnd = as.integer(NA), 
    type = "weeks", rainbow = TRUE)
plotAllABT(objName = "inData", dayStart = 140, dayEnd = 350, 
    type = "weeks", rainbow = TRUE)
plotAllABT(objName = "inData", dayStart = 140, dayEnd = 350, 
    type = "days", rainbow = TRUE)

# orderOfCategories, lowessSmooth
inDataA <- groupAttribABT(objName = "inData")
plotAllABT(objName = "inDataA", 
    type = "weeks", lineAttrib = "AgeGroup", addPoints = TRUE, lowessSmooth = TRUE)
plotAllABT(objName = "inDataA", 
    type = "weeks", lineAttrib = "AgeGroup", addPoints = TRUE, lowessSmooth = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"))

# lineForPre, lineFor1st
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = FALSE, lineFor1st = FALSE)
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = TRUE, lineFor1st = TRUE)
    
# omitPreVac (type = "days")
plotAllABT(objName = "inDataA", 
    type = "days", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), omitPreVac = FALSE)
plotAllABT(objName = "inDataA", 
    type = "days", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), omitPreVac = TRUE)
    
# omitPreVac (type = "M1")
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = TRUE, lineFor1st = TRUE, omitPreVac = FALSE)
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = FALSE, lineFor1st = TRUE, omitPreVac = TRUE)
    
# lineColDark (type = "days")
plotAllABT(objName = "inDataA", 
    type = "days", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), lineColDark = FALSE)
plotAllABT(objName = "inDataA", 
    type = "days", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), lineColDark = TRUE)
    
# lineColDark (type = "M1")
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = TRUE, lineFor1st = TRUE, lineColDark = FALSE)
plotAllABT(objName = "inDataA", 
    type = "M1", lineAttrib = "AgeGroup", addPoints = TRUE, 
    orderOfCategories = c("young", "middle", "elderly"), geometricMean = TRUE, 
    lineForPre = TRUE, lineFor1st = TRUE, lineColDark = TRUE)
}

\value{
No object will be returned. Instead, this function will drow a plot in a graphic device, or save a PDF file in the current directory.
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

