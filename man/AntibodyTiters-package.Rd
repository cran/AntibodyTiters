\name{AntibodyTiters-package}
\alias{AntibodyTiters-package}
\alias{AntibodyTiters}
\docType{package}

\title{
Antibody Titer Analysis of Vaccinated Patients.
}

\description{
  Visualization of antibody titer scores is valuable 
  for examination of vaccination effects. \pkg{AntibodyTiters} 
  visualizes antibody titers of all or selected patients. 
  Users must use excel files with a specified format. 
}

\details{
  \tabular{ll}{
    Package: \tab AntibodyTiters\cr
    Type: \tab Package\cr
    Version: \tab 0.1.4\cr
    Date: \tab 2021-09-30\cr
    License: \tab GPL-3\cr
  }
  \code{\link{emptyABT}}: Production of an empty xlsx file 
    in which experimental results should be recorded.

  \code{\link{idABT}}: Extracting patient IDs from the dataset.

  \code{\link{plotAllABT}}: Plotting antibody titers of All 
    patients.

  \code{\link{plotEachABT}}: Plotting antibody titers of each 
    patient.

  \code{\link{readABT}}: Reading an xlsx file 
    and examining the validity of the dataset.

  \code{\link{toyABT}}: Production of an xlsx file with toy data. 
}

\author{
  Yuuki Mizumoto, Hiroaki Kato, Takeshi Urano\cr
  Maintainer: Hiroaki Kato <hkato@med.shimane-u.ac.jp>
}

\keyword{ package }
