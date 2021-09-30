\name{emptyABT}
\alias{emptyABT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Production of an empty xlsx file 
    in which experimental results should be recorded.
}
\description{
    \pkg{AntibodyTiters} requires the input xlsx files be 
    in a specific format. This function produces an empty 
    xlsx file in the format.
}
\usage{emptyABT(fileName = "empty.xlsx", pmax = 7, returnDF = FALSE)}

\arguments{
    \item{fileName}{a file name to be produced.}
    \item{pmax}{an integer (>=4 & <18) for specifying 
        the maximam sampling point.}
    \item{returnDF}{a logical value indicating 
        whether a data.frame is returned 
        instead of being stored as an xlsx file.}
}

\examples{
# emptyABT(fileName = "empty.xlsx", pmax = 7, returnDF = FALSE)
# The above command will create an empty excel file named empty.xlsx 
# in the current directory. 
}

\value{
When the argument \code{returnDF} is \code{FALSE} (default), an xlsx file will be stored in the current directory. When \code{returnDF} is \code{TRUE}, a data.frame will be returned.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}

