\name{AntibodyTiters-package}
\alias{AntibodyTiters-package}
\alias{AntibodyTiters}
\docType{package}

\title{
Antibody Titer Analysis of Vaccinated Patients.
}

\description{
  Visualization of antibody titer scores is valuable for examination of vaccination effects. \pkg{AntibodyTiters} visualizes antibody titers of all or selected patients. This package also produces empty excel files in a specified format, in which users can fill in experimental data for visualization. Excel files with toy data can also be produced, so that users can see how it is visualized before obtaining real data. The data should contain titer scores at pre-vaccination, after-1st shot, after-2nd shot, and at least one additional sampling points. Patients with missing values can be included. The first two sampling points (pre-vaccination and after-1st shot) will be plotted discretely, whereas those following will be plotted on a continuous time scale that starts from the day of second shot. Half-life of titer can also be calculated for each pair of sampling points.
}

\details{
  \tabular{ll}{
    Package: \tab AntibodyTiters\cr
    Type: \tab Package\cr
    Version: \tab 0.1.24\cr
    Date: \tab 2022-05-24\cr
    License: \tab GPL-3\cr
  }
  \code{\link{emptyABT}}: Production of an empty xlsx file 
    in which experimental results should be recorded.

  \code{\link{groupAttribABT}}: Addition of a new attribute that 
    categorizes patients in specified groups to an ABT class object. 

  \code{\link{idABT}}: Extracting patient IDs from an ABT class object.

  \code{\link{plotAllABT}}: Plotting antibody titers of All 
    patients.

  \code{\link{plotEachABT}}: Plotting antibody titers of each 
    patient.

  \code{\link{readABT}}: Reading an xlsx file 
    and examining the validity of the dataset.

  \code{\link{halfLifeABT}}: Calculating half-life from an ABT class object.

  \code{\link{toyABT}}: Production of an xlsx file with toy data. 
}

\author{
  Yuuki Mizumoto, Hiroaki Kato, Takeshi Urano\cr
  Maintainer: Hiroaki Kato <hkato@med.shimane-u.ac.jp>
}

\keyword{ package }
