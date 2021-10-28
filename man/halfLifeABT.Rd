\name{halfLifeABT}
\alias{halfLifeABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Calculating half-life from an ABT class object.
}
\description{
    This function calculates T1/2 for each set of sampling points.
}
\usage{halfLifeABT(x, output = "list", OutFileName = "Thalf.xlsx")}

\arguments{
    \item{x}{an ABT class object.}
    \item{output}{a character specifying the type of output; "list", "data.frame" or "xlsx".}
    \item{OutFileName}{a character specifying the file name to be stored in the current directory when \code{output = "xlsx"}.}
}

\examples{
inData <- readABT(fileName = system.file("extdata","toy.xlsx",package="AntibodyTiters"))
inDataVL <- subset(inData, VeryLow == TRUE)
halfLifeABT(inDataVL, output = "data.frame")
}

\value{
a list, a data.frame or an xlsx file depending on the \code{output} argument. The list object (produced with \code{output="list"}) contains data.frame objects for each patients, whereas the data.frame object (produced with \code{output="data.frame"}) is a combined form derived from the list (simply produced with rbind). The data.frames contain columns named \code{ID} (patient ID), \code{START} (starting point), \code{END} (end point), \code{SPAN} (t), \code{MID} (mid point), \code{C0} (initial titer), \code{Ct} (titer at t) and \code{Thalf} (T1/2).  
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

